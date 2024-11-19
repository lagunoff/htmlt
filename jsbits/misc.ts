import { EvalState, EvalContext } from "./proto";
import * as proto from "./proto";


export function evalUint8Array(command: Uint8Array, options?: Partial<EvalContext>): unknown {
  const context: EvalContext = {
    refs: options?.refs || new Map(),
    stack: options?.stack || null,
    triggerEvent: options?.triggerEvent || function() {},
    resumeCont: options?.resumeCont || function() {},
  };
  const mem = new DataView(command.buffer);

  const intp: EvalState = {
    context,
    mem,
    isMutableMem: false,
    begin: 0,
    end: command.byteLength,
  };
  return proto.evalMem(intp);
}

export function evalBase64(command: string, options?: Partial<EvalContext>): unknown {
  const u8array = Uint8Array.from(atob(command), function(c) { return c.charCodeAt(0); });
  return evalUint8Array(u8array, options);
}
