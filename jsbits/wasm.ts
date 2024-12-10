import { WASI, File, OpenFile } from '@bjorn3/browser_wasi_shim';
import { ClientMsgTag, EventId, EvalState, Ptr, EvalContext } from "./proto";
import * as proto from "./proto";

let inst: WebAssembly.Instance;

export function runWasm(wasmUri: string, startFlags: unknown = null, options?: Partial<EvalContext>) {
  const context: EvalContext = {
    refs: options?.refs || new proto.RefStore(),
    stack: options?.stack || null,
    triggerEvent: function() {},
    resumeCont: function() {},
  };
  // const stdout = new OpenFile(new File([]));
  // const stderr = new OpenFile(new File([]));
  const stdout = new OpenFileDebug(new File([]), consoleLineBuffering(console.log));
  const stderr = new OpenFileDebug(new File([]), consoleLineBuffering(console.log));
      // @ts-ignore
  window['stdout'] = stdout;
      // @ts-ignore
  window['stderr'] = stderr;

  const wasi = new WASI([], [], [
    new OpenFile(new File([])), // stdin
    stdout as any,
    stderr as any,
  ]);

  function clickable_eval_buffer(ptr: Ptr, len: number): void {
    // @ts-ignore
    const context1 = {...context, resumeCont, triggerEvent}

    function triggerEvent(eventId: EventId, arg: unknown) {
    // @ts-ignore
    const mem = new DataView(inst.exports.memory.buffer);
      const encoderState = {mem, begin: ptr, end: ptr + len};
      proto.encodeClientMessage(encoderState, [ClientMsgTag.EventMsg, eventId, arg]);
      // @ts-ignore
      inst.exports.wasm_app(ptr);
    };

    function resumeCont(contId: number, res: unknown) {
    // @ts-ignore
      const mem = new DataView(inst.exports.memory.buffer);
      const encoderState = {mem, begin: ptr, end: ptr + len};
      proto.encodeClientMessage(encoderState, [ClientMsgTag.ResumeMsg, contId, res]);
      // @ts-ignore
      inst.exports.wasm_app(ptr);
    };

    // @ts-ignore
    const mem = new DataView(inst.exports.memory.buffer);
    const intp: EvalState = {
      context: context1,
      mem,
      isMutableMem: true,
      begin: ptr,
      end: ptr + len,
    };

    proto.evalMem(intp);
  }

  function console_log(ptr: Ptr, len: number): void {
    // @ts-ignore
    const strView = new Uint8Array(inst.exports.memory.buffer).subarray(ptr, ptr + len);
    return console.log(new TextDecoder('utf8').decode(strView));
  }

  WebAssembly.compileStreaming(fetch(wasmUri))
    .then(wasm => WebAssembly.instantiate(wasm, {
      wasi_snapshot_preview1: wasi.wasiImport,
      env: {
        clickable_eval_buffer,
        console_log,
      },
    }))
    .then(inst1 => {
      inst = inst1;
      // @ts-ignore
      wasi.initialize(inst1);
      // @ts-ignore
      inst1.exports.hs_init();
      // @ts-ignore
      const buf: number = inst1.exports.wasm_app(0);
      // @ts-ignore
      const mem = new DataView(inst1.exports.memory.buffer);
      const encoderState = {mem, begin: buf, end: buf + (1024 * 100)};
      proto.encodeClientMessage(encoderState, [ClientMsgTag.StartMsg, startFlags]);
      // @ts-ignore
      inst1.exports.wasm_app(buf);
      window.addEventListener('beforeunload', () => {
        // @ts-ignore
        const mem = new DataView(inst1.exports.memory.buffer);
        const encoderState = {mem, begin: buf, end: buf + (1024 * 100)};
        proto.encodeClientMessage(encoderState, [ClientMsgTag.BeforeUnloadMsg]);
        // @ts-ignore
        inst1.exports.wasm_app(buf);
      });
    });
};

export class OpenFileDebug extends OpenFile {
  public printDebug: (s: Uint8Array) => void;

  constructor(file: File, printDebug: (s: Uint8Array) => void) {
    super(file);
    this.printDebug = printDebug;
  }

  fd_write(data: Uint8Array): {
    ret: number;
    nwritten: number;
  } {
    this.printDebug(data);
    return super.fd_write(data);
  }

  fd_pwrite(data: Uint8Array, offset: bigint): {
    ret: number;
    nwritten: number;
  } {
    this.printDebug(data.subarray(Number(offset)));
    return super.fd_pwrite(data, offset);
  }
}

// Split given chunks of memory into lines and pass decoded strings
// into the given logger function, buffer unfinished lines if neccessary
function consoleLineBuffering(logger: (s: string) => void): (u8: Uint8Array) => void {
  let buffer: Uint8Array[] = [];
  return (u8: Uint8Array) => {
    const go = (bytes: Uint8Array) => {
      if (bytes.byteLength == 0) return;
      const newLineIndex = bytes.findIndex(b => b == '\n'.charCodeAt(0));
      if (newLineIndex >= 0) {
        const olderBits = buffer.map(u8s => new TextDecoder('utf8').decode(u8s)).join('');
        buffer = [];
        const lastBits = new TextDecoder('utf8').decode(bytes.subarray(0, newLineIndex));
        logger(olderBits + lastBits);
        go(bytes.subarray(newLineIndex + 1));
      } else {
        buffer.push(bytes.slice());
      }
    };
    go(u8);
  };
}
