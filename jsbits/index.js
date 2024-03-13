import { WASI, File, OpenFile } from '@bjorn3/browser_wasi_shim';
import * as jsffi from './jsffi';

let __exports = {};

window.startReactor = async function (wasmUri, opt) {
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
  await wasi.initialize(inst);
  await inst.exports.hs_init();
  await inst.exports.wasm_main();
};
