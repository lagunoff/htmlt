import { WASI, File, OpenFile } from '@bjorn3/browser_wasi_shim';
// @ts-ignore
import * as jsffi from './jsffi';
import * as p from './protocol';
import { JavaScriptMessage, HaskellMessageTag, List, JavaScriptMessageTag } from './protocol';

export type HaskellPointer = number;

export type HaskellExports = {
  hs_init: () => void;
  wasm_app: (flags: HaskellPointer) => void;
  malloc: (size: number) => HaskellPointer;
  memory: WebAssembly.Memory;
};

export function loadBuffer(exports: HaskellExports, ptr: HaskellPointer): Uint8Array {
  const b = new Uint8Array(exports.memory.buffer, ptr);
  const len = b[0] +
    (b[1] << 8) +
    (b[2] << 16) +
    (b[3] << 24) +
    (b[4] << 32) +
    (b[5] << 40) +
    (b[6] << 48) +
    (b[7] << 56);
  const buf = (new Uint8Array(exports.memory.buffer, ptr + 8, len)).slice().buffer;
  return new Uint8Array(buf);
}

export function storeBuffer(exports: HaskellExports, u8array: Uint8Array): HaskellPointer {
  const len = u8array.byteLength;
  const ptr = exports.malloc(u8array.length + 8);
  // Write the length of the buffer as 8 bytes before the buffer
  const view = new DataView(exports.memory.buffer);
  view.setUint32(ptr, len, true);

  // Copy the buffer into WebAssembly memory
  const dest = new Uint8Array(exports.memory.buffer, ptr + 8, len);
  dest.set(u8array);
  return ptr;
}

export function evalMessage(exports: HaskellExports, ptr: HaskellPointer): HaskellPointer {
  const inbuf = loadBuffer(exports, ptr);
  const haskMsg = p.haskellMessage.decode(inbuf);
  const jsCallback = (jsmsg: JavaScriptMessage, _argScope: List<IArguments>) => {
    const outbuf = p.javascriptMessage.encode(jsmsg);
    const ptr = storeBuffer(exports, outbuf);
    exports.wasm_app(ptr);
  };
  switch (haskMsg.tag) {
    case HaskellMessageTag.EvalExpr: {
      const result = p.evalExpr(jsCallback, [globalThis, null], null, haskMsg.expr);
      const jsmsg: JavaScriptMessage = { tag: JavaScriptMessageTag.Return, 0: p.unknownToValue(result) };
      const outbuf = p.javascriptMessage.encode(jsmsg);
      return storeBuffer(exports, outbuf);
    }
    case HaskellMessageTag.HotReload: {
      window.location.reload();
      const jsmsg: JavaScriptMessage = { tag: JavaScriptMessageTag.Return, 0: p.unknownToValue(null) };
      const outbuf = p.javascriptMessage.encode(jsmsg);
      return storeBuffer(exports, outbuf);
    }
    case HaskellMessageTag.Halt: {
      const jsmsg: JavaScriptMessage = { tag: JavaScriptMessageTag.Return, 0: p.unknownToValue(null) };
      const outbuf = p.javascriptMessage.encode(jsmsg);
      return storeBuffer(exports, outbuf);
    }
  }
}

let __exports: HaskellExports = {} as any;

export async function startWasm(wasmUri: string, startFlags: unknown = null) {
  const wasi = new WASI([], [], [
    new OpenFile(new File([])), // stdin
    new OpenFile(new File([])), // stdout
    new OpenFile(new File([])), // stderr
  ]);

  const wasm = await WebAssembly.compileStreaming(fetch(wasmUri));
  const inst = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffi.default(__exports)
  });

  Object.assign(__exports, inst.exports);
  // @ts-ignore
  wasi.initialize(inst);
  // @ts-ignore
  await inst.exports.hs_init();

  const startFlagsValue = p.unknownToValue(startFlags);
  const startFlagsMessage: JavaScriptMessage = { tag: JavaScriptMessageTag.Start, 0: startFlagsValue };
  const startFlagsBuffer = p.javascriptMessage.encode(startFlagsMessage);
  const startFlagsPtr = storeBuffer(__exports, startFlagsBuffer);

  // @ts-ignore
  await inst.exports.wasm_app(startFlagsPtr);

  window.addEventListener("beforeunload", () => {
    const unloadMessage: JavaScriptMessage = { tag: JavaScriptMessageTag.BeforeUnload };
    const unloadBuffer = p.javascriptMessage.encode(unloadMessage);
    const unloadPtr = storeBuffer(__exports, unloadBuffer);
    __exports.wasm_app(unloadPtr);
  });
};
