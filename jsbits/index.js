import * as websocket from './websocket';
import * as wasm from './wasm';

window.clickable = {
  runWebsocket: websocket.runWebsocket,
  runWasm: wasm.runWasm,
};
