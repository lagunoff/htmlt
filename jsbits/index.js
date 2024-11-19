import * as websocket from './websocket';
import * as wasm from './wasm';
import * as misc from './misc';

window.clickable = {
  runWebsocket: websocket.runWebsocket,
  runWasm: wasm.runWasm,
  evalUint8Array: misc.evalUint8Array,
  evalBase64: misc.evalBase64,
};
