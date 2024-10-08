import { WASI, File, OpenFile } from '@bjorn3/browser_wasi_shim';
import { ClientMsgTag, EventId, EvalState, PersistentState, Ptr } from "./proto";
import * as proto from "./proto";

let inst: WebAssembly.Instance;

export function runWasm(wasmUri: string, startFlags: unknown = null) {
  const wasi = new WASI([], [], [
    new OpenFile(new File([])), // stdin
    new OpenFile(new File([])), // stdout
    new OpenFile(new File([])), // stderr
  ]);

  const persistent: PersistentState = {
    refs: new Map(),
    stack: null,
  };

  function clickable_eval_buffer(ptr: Ptr, len: number): void {
    // @ts-ignore
    const mem = new DataView(inst.exports.memory.buffer);

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
      triggerEvent,
      resumeCont,
      persistent,
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
