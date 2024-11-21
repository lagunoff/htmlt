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

  const wasi = new WASI([], [], [
    new OpenFile(new File([])), // stdin
    new OpenFile(new File([])), // stdout
    new OpenFile(new File([])), // stderr
  ]);

  function clickable_eval_buffer(ptr: Ptr, len: number): void {
    // @ts-ignore
    const mem = new DataView(inst.exports.memory.buffer);
    const context1 = {...context, resumeCont, triggerEvent}

    function triggerEvent(eventId: EventId, arg: unknown) {
      const encoderState = {mem, begin: ptr, end: ptr + len};
      proto.encodeClientMessage(encoderState, [ClientMsgTag.EventMsg, eventId, arg]);
      // @ts-ignore
      inst.exports.wasm_app(ptr);
    };

    function resumeCont(contId: number, res: unknown) {
      const encoderState = {mem, begin: ptr, end: ptr + len};
      proto.encodeClientMessage(encoderState, [ClientMsgTag.ResumeMsg, contId, res]);
      // @ts-ignore
      inst.exports.wasm_app(ptr);
    };

    const intp: EvalState = {
      context: context1,
      mem,
      isMutableMem: true,
      begin: ptr,
      end: ptr + len,
    };

    proto.evalMem(intp);
  }

  WebAssembly.compileStreaming(fetch(wasmUri))
    .then(wasm => WebAssembly.instantiate(wasm, {
      wasi_snapshot_preview1: wasi.wasiImport,
      env: {
        clickable_eval_buffer,
      },
    }))
    .then(inst1 => {
      inst = inst1;
      // @ts-ignore
      wasi.initialize(inst1);
      // @ts-ignore
      inst1.exports.hs_init();
      // @ts-ignore
      inst1.exports.wasm_app(0);
    });
};
