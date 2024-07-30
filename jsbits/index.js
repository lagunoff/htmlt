import * as dev from './dev';
import * as protocol from './protocol';
import * as wasm from './wasm';

// Inject ClickablePublic object into the global scope
window.clickable = {
  startWasm: wasm.startWasm,
  startDev: dev.startDev,
  evalExpr: protocol.evalExpr,
  evalUint8Array: protocol.evalUint8Array
};
