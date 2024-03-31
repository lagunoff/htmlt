import * as ffi from './ffi';
import * as devserver from './devserver';

window.startWasm = ffi.startWasm;
window.startClient = devserver.startClient;
