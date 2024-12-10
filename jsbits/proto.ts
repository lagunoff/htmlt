/**
 * A recursive type representing a linked list, where `List<T>` is
 * either `null` (empty) or a `Cons` (non-empty list).
 *
 * @template T - The type of elements in the list.
 */
export type List<T> = null | Cons<T>;

/**
 * A tuple representing a non-empty list node, where the first element
 * is the value (`T`), and the second is the rest of the list
 * (`List<T>`).
 *
 * @template T - The type of the element.
 */
export type Cons<T> = [T, List<T>];

export const VAL = 0;

export const NEXT = 1;

/**
 * Creates a `Cons` (list node) with the given value and tail (rest of
 * the list).
 *
 * @template T - The type of the element.
 * @param {T} val - The value for the new node.
 * @param {List<T>} tail - The remaining list.
 * @returns {Cons<T>} - A new list node.
 */
export function Cons<T>(val: T, tail: List<T>): Cons<T> {
  return [val, tail];
}

/** Discriminator for `Expr` */
export enum ExprTag {
  Null,
  Bool,
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  Str,
  Arr,
  Obj,
  U8Arr,

  Dot,
  SetProp,
  Ix,
  Id,

  Lam,
  Arg,
  Apply,
  Call,

  AssignRef,
  Ref,
  FreeScope,
  MoveScope,

  PeekStack,
  PushStack,
  PopStack,

  PopIns,
  ElementProp,
  ElementAttr,
  ClassListAdd,
  ClassListRemove,
  InsertPlaceholder,
  InsertPlaceholderBefore,
  ClearPlaceholder,
  DetachPlaceholder,

  CreateElement,
  CreateElementNS,
  CreateText,
  UpdateText,

  Eval,
  TriggerEvent,
  Resume,
  Out,
}

/** Encode `Expr` type as a union of disjoint n-tuples, keep in
 * sync with Haskell definition in
 * https://github.com/lagunoff/clickable/blob/master/Clickable/Types.hs */
export type Expr =
  | [ExprTag.Null]
  | [ExprTag.Bool, number]
  | [ExprTag.I8, number]
  | [ExprTag.I16, number]
  | [ExprTag.I32, number]
  | [ExprTag.I64, bigint]
  | [ExprTag.U8, number]
  | [ExprTag.U16, number]
  | [ExprTag.U32, number]
  | [ExprTag.U64, bigint]
  | [ExprTag.F32, number]
  | [ExprTag.F64, number]
  | [ExprTag.Str, string]
  | [ExprTag.Arr, Expr[]]
  | [ExprTag.Obj, [string, Expr][]]
  | [ExprTag.U8Arr, Uint8Array]

  | [ExprTag.Lam, Expr]
  | [ExprTag.Arg, number]
  | [ExprTag.Apply, Expr, [Expr]]
  | [ExprTag.Call, Expr, string, [Expr]]

  | [ExprTag.AssignRef, number, number, Expr]
  | [ExprTag.Ref, number]
  | [ExprTag.FreeScope, number]
  | [ExprTag.MoveScope, number, number]

  | [ExprTag.PeekStack, number]
  | [ExprTag.PushStack, Expr]
  | [ExprTag.PopStack]

  | [ExprTag.PopIns, Expr, Expr]
  | [ExprTag.ElementProp, string, Expr]
  | [ExprTag.ElementAttr, string, string]
  | [ExprTag.ClassListAdd, Expr, string[]]
  | [ExprTag.ClassListRemove, Expr, string[]]
  | [ExprTag.InsertPlaceholder]
  | [ExprTag.InsertPlaceholderBefore, Expr]
  | [ExprTag.ClearPlaceholder, Expr]
  | [ExprTag.DetachPlaceholder, Expr]

  | [ExprTag.CreateElement, string]
  | [ExprTag.CreateElementNS, string, string]
  | [ExprTag.CreateText, string]
  | [ExprTag.UpdateText, Expr, string]

  | [ExprTag.Eval, string]
  | [ExprTag.TriggerEvent, number, Expr]
  | [ExprTag.Resume, number, Expr]
  | [ExprTag.Out]
  ;

export type Ptr = number;
export type ScopeId = number;
export type RefId = number;
export type EventId = number;

export type EvalState = {
  readonly context: EvalContext;
  readonly mem: DataView;
  readonly isMutableMem: boolean;
  begin: Ptr;
  readonly end: Ptr;
};

export type EvalContext = {
  readonly triggerEvent: (eventId: EventId, arg: unknown) => void;
  readonly resumeCont: (contId: number, res: unknown) => void;
  readonly refs: RefStore;
  stack: List<unknown>;
};

/** Evaluate sequence of `Expr` encoded as `Data.Binary.Binary`
 * instance inside the memory given in `self.mem` */
export function evalMem(self: EvalState): unknown {
  let res = null;
  for (; self.begin < self.end;) {
    res = evalNext(self, null, res);
  }
  return res;
}

/** Evaluate next single `Expr` relative to `self.begin` pointer */
export function evalNext(self: EvalState, args: List<unknown> = null, prevRes: unknown = null): unknown {
  const tag: ExprTag = self.mem.getInt8(self.begin);
  self.begin++;

  switch (tag) {
    case ExprTag.Null: {
      return null;
    };
    case ExprTag.Bool: {
      const res = self.mem.getInt8(self.begin) ? true : false;
      self.begin++;
      return res;
    };

    case ExprTag.I8: {
      const res = self.mem.getInt8(self.begin);
      self.begin++;
      return res;
    };
    case ExprTag.I16: {
      const res = self.mem.getInt16(self.begin, false);
      self.begin += 2;
      return res;
    };
    case ExprTag.I32: {
      const res = self.mem.getInt32(self.begin, false);
      self.begin += 4;
      return res;
    };
    case ExprTag.I64: {
      const res = self.mem.getBigInt64(self.begin, false);
      self.begin += 8;
      return res;
    };

    case ExprTag.U8: {
      const res = self.mem.getUint8(self.begin);
      self.begin++;
      return res;
    };
    case ExprTag.U16: {
      const res = self.mem.getUint16(self.begin, false);
      self.begin += 2;
      return res;
    };
    case ExprTag.U32: {
      const res = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      return res;
    };
    case ExprTag.U64: {
      const res = self.mem.getBigUint64(self.begin, false);
      self.begin += 8;
      return res;
    };

    case ExprTag.F32: {
      const res = self.mem.getFloat32(self.begin, true);
      self.begin += 4;
      return res;
    };
    case ExprTag.F64: {
      const res = self.mem.getFloat64(self.begin, true);
      self.begin += 8;
      return res;
    };

    case ExprTag.Str: {
      return decodeString(self);
    };
    case ExprTag.Arr: {
      const len = Number(self.mem.getBigUint64(self.begin, false));
      const res = new Array(len).fill(undefined);
      self.begin += 8;
      for (let i = 0; i < len; i++) {
        const val = evalNext(self, args, prevRes);
        res[i] = val;
      }
      return res;
    };
    case ExprTag.Obj: {
      const len = Number(self.mem.getBigUint64(self.begin, false));
      const res: Record<string, unknown> = {};
      self.begin += 8;

      for (let i = 0; i < len; i++) {
        const key = decodeString(self);
        const val = evalNext(self, args, prevRes);

        res[key] = val;
      }
      return res;
    };
    case ExprTag.U8Arr: {
      const len = Number(self.mem.getBigUint64(self.begin, false));
      self.begin += 8;
      const res = new Uint8Array(self.mem.buffer).slice(self.begin, self.begin + len);
      self.begin += len;
      return res;
    };

    case ExprTag.Dot: {
      const obj = evalNext(self, args, prevRes);
      const key = decodeString(self);
      return (obj as any)[key];
    };
    case ExprTag.SetProp: {
      const obj = evalNext(self, args, prevRes);
      const key = decodeString(self);
      const val = evalNext(self, args, prevRes);
      (obj as any)[key] = val;
      return null;
    };
    case ExprTag.Ix: {
      const obj = evalNext(self, args, prevRes);
      const key = self.mem.getUint32(self.begin);
      self.begin += 4;
      return (obj as any)[key];
    };
    case ExprTag.Id: {
      const iden = decodeString(self);
      return (global as any)[iden];
    };

    case ExprTag.Lam: {
      const ptrEnd = lookaheadNext(self.mem, self.begin);
      if (self.isMutableMem) {
        const clonedBody = new Uint8Array(self.mem.buffer).slice(self.begin, ptrEnd);
        const ctxNew: EvalState = {
          ...self,
          mem: new DataView(clonedBody.buffer),
          isMutableMem: false,
          begin: 0,
          end: clonedBody.length,
        };
        self.begin = ptrEnd;
        return function (arg: unknown) {
          ctxNew.begin = 0;
          evalNext(ctxNew, Cons(arg, args));
        };
      } else {
        const resetBegin = self.begin;
        const ctxNew: EvalState = {
          ...self,
          end: ptrEnd,
        };
        self.begin = ptrEnd;
        return function (arg: unknown) {
          ctxNew.begin = resetBegin;
          evalNext(ctxNew, Cons(arg, args));
        };
      }
    };
    case ExprTag.Arg: {
      const argIx = self.mem.getInt8(self.begin);
      self.begin += 1;
      let iter = args;
      let i = 0;
      while (iter) {
        if (i == argIx) {
          return iter[VAL];
        }
        iter = iter[NEXT];
        i++;
      }
      throw new Error("Arg: index out of argument stack");
    };
    case ExprTag.Apply: {
      const fn = evalNext(self, args, prevRes) as Function;
      const fnArgNum = Number(self.mem.getBigUint64(self.begin, false));
      self.begin += 8;
      const fnArgs = new Array(fnArgNum).fill(null);
      for (let i = 0; i < fnArgNum; i++) {
        fnArgs[i] = evalNext(self, args, prevRes);
      }
      return (fn).apply(undefined, fnArgs);
    };
    case ExprTag.Call: {
      const obj = evalNext(self, args, prevRes);
      const key = decodeString(self);
      const callArgNum = Number(self.mem.getBigUint64(self.begin, false));
      self.begin += 8;
      const callArgs = new Array(callArgNum).fill(null);
      for (let i = 0; i < callArgNum; i++) {
        callArgs[i] = evalNext(self, args, prevRes);
      }
      return (obj as any)[key].apply(obj, callArgs);
    };

    case ExprTag.AssignRef: {
      const scope = self.mem.getUint32(self.begin, false);
      const ref = self.mem.getUint32(self.begin + 4, false);
      self.begin += 8;
      const val = evalNext(self, args, prevRes);
      self.context.refs.assignRef(scope, ref, val);
      return val;
    };
    case ExprTag.Ref: {
      const ref = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      return self.context.refs.refs.get(ref);
    };
    case ExprTag.FreeScope: {
      const scope = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      self.context.refs.freeScope(scope);
      return null;
    };
    case ExprTag.MoveScope: {
      const src = self.mem.getUint32(self.begin, false);
      const dest = self.mem.getUint32(self.begin + 4, false);
      self.begin += 8;
      self.context.refs.moveScope(src, dest);
      return null;
    };

    case ExprTag.PeekStack: {
      const stackIx = self.mem.getInt8(self.begin);
      self.begin += 1;
      let iter = self.context.stack;
      let i = 0;
      while (iter) {
        if (i == stackIx) {
          return iter[VAL];
        }
        iter = iter[NEXT];
        i++;
      }
      throw new Error("PeekStack: index out of stack size");
    };
    case ExprTag.PushStack: {
      const val = evalNext(self, args, prevRes);
      self.context.stack = Cons(val, self.context.stack);
      return val;
    };
    case ExprTag.PopStack: {
      if (!self.context.stack) {
        throw new Error("PopStack: empty stack");
      }
      const res = self.context.stack[VAL];
      self.context.stack = self.context.stack[NEXT];
      return res;
    };

    case ExprTag.PopIns: {
      if (!self.context.stack) {
        throw new Error("PopIns: empty stack");
      }
      const tip = self.context.stack[VAL] as Node;
      self.context.stack = self.context.stack[NEXT];
      if (!self.context.stack) {
        throw new Error("PopIns: empty stack");
      }
      const parent = self.context.stack[VAL] as Element;
      utils.insert(parent, tip);
      return tip;
    };
    case ExprTag.ElementProp: {
      const el = evalNext(self, args, prevRes);
      const key = decodeString(self);
      const val = evalNext(self, args, prevRes);
      utils.prop(el as any, key, val);
      return null;
    };
    case ExprTag.ElementAttr: {
      const el = evalNext(self, args, prevRes);
      const key = decodeString(self);
      const val = decodeString(self);
      utils.attr(el as any, key, val);
      return null;
    };
    case ExprTag.ClassListAdd: {
      const el = evalNext(self, args, prevRes) as Element;
      const className = decodeString(self);
      return el.classList.add(className);
    };
    case ExprTag.ClassListRemove: {
      const el = evalNext(self, args, prevRes) as Element;
      const className = decodeString(self);
      return el.classList.remove(className);
    };
    case ExprTag.InsertPlaceholder: {
      if (!self.context.stack) {
        throw new Error("InsertPlaceholder: empty stack");
      }
      const el = self.context.stack[VAL] as any;
      return utils.insertPlaceholder(el);
    };
    case ExprTag.InsertPlaceholderBefore: {
      if (!self.context.stack) {
        throw new Error("InsertPlaceholderBefore: empty stack");
      }
      const el = self.context.stack[VAL] as any;
      const anchor = evalNext(self, args, prevRes) as Element;
      return utils.insertPlaceholderBefore(el, anchor);
    };
    case ExprTag.ClearPlaceholder: {
      const node = evalNext(self, args, prevRes);
      utils.clearPlaceholder(node as any, false);
      return null;
    };
    case ExprTag.DetachPlaceholder: {
      const node = evalNext(self, args, prevRes);
      utils.clearPlaceholder(node as any, true);
      return null;
    };
    case ExprTag.CreateElement: {
      const tagName = decodeString(self);
      return document.createElement(tagName);
    };
    case ExprTag.CreateElementNS: {
      const tagName = decodeString(self);
      const ns = decodeString(self);
      return document.createElementNS(tagName, ns);
    };
    case ExprTag.CreateText: {
      const content = decodeString(self);
      return document.createTextNode(content);
    };
    case ExprTag.UpdateText: {
      const node = evalNext(self, args, prevRes);
      const content = decodeString(self);
      (node as Text).textContent = content;
      return null;
    };
    case ExprTag.Eval: {
      const js = decodeString(self);
      return eval(js);
    };
    case ExprTag.TriggerEvent: {
      const eventId = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      const pload = evalNext(self, args, prevRes);
      self.context.triggerEvent(eventId, pload);
      return null;
    };
    case ExprTag.Resume: {
      const contId = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      const pload = evalNext(self, args, prevRes);
      self.context.resumeCont(contId, pload);
      return null;
    };
    case ExprTag.Out: {
      return prevRes;
    };
  }
}

/** Parse next Expr without evaluating it. Required to clone lambda
 * body to be able to run it later
 */
export function lookaheadNext(mem: DataView, ptr: Ptr): Ptr {
  const tag: ExprTag = mem.getInt8(ptr++);

  switch (tag) {
    case ExprTag.Null: {
      return ptr;
    };
    case ExprTag.Bool: {
      return ptr + 1;
    };

    case ExprTag.I8: {
      return ptr + 1;
    };
    case ExprTag.I16: {
      return ptr + 2;
    };
    case ExprTag.I32: {
      return ptr + 4;
    };
    case ExprTag.I64: {
      return ptr + 8;
    };

    case ExprTag.U8: {
      return ptr + 1;
    };
    case ExprTag.U16: {
      return ptr + 2;
    };
    case ExprTag.U32: {
      return ptr + 4;
    };
    case ExprTag.U64: {
      return ptr + 8;
    };

    case ExprTag.F32: {
      return ptr + 4;
    };
    case ExprTag.F64: {
      return ptr + 8;
    };

    case ExprTag.Str: {
      const len = Number(mem.getBigUint64(ptr, false));
      return ptr + 8 + len;
    };
    case ExprTag.Arr: {
      const len = Number(mem.getBigUint64(ptr, false));
      let iter = ptr + 8;
      for (let i = 0; i < len; i++) {
        iter = lookaheadNext(mem, iter);
      }
      return iter;
    };
    case ExprTag.Obj: {
      const len = Number(mem.getBigUint64(ptr, false));
      let iter = ptr + 8;

      for (let i = 0; i < len; i++) {
        iter = lookaheadString(mem, iter);
        iter = lookaheadNext(mem, iter);
      }
      return iter;
    };
    case ExprTag.U8Arr: {
      const len = Number(mem.getBigUint64(ptr, false));
      return ptr + 8 + len;
    };

    case ExprTag.Dot: {
      const newPtr = lookaheadNext(mem, ptr);
      return lookaheadString(mem, newPtr);
    };
    case ExprTag.SetProp: {
      const newPtr0 = lookaheadNext(mem, ptr);
      const newPtr1 = lookaheadString(mem, newPtr0);
      return lookaheadNext(mem, newPtr1);
    };
    case ExprTag.Ix: {
      const newPtr = lookaheadNext(mem, ptr);
      return newPtr + 4;
    };
    case ExprTag.Id: {
      return lookaheadString(mem, ptr);
    };

    case ExprTag.Lam: {
      return lookaheadNext(mem, ptr);
    };
    case ExprTag.Arg: {
      return ptr + 1;
    };
    case ExprTag.Apply: {
      const newPtr0 = lookaheadNext(mem, ptr);
      return lookaheadNext(mem, newPtr0);
    };
    case ExprTag.Call: {
      const newPtr0 = lookaheadNext(mem, ptr);
      const newPtr1 = lookaheadString(mem, newPtr0);
      return lookaheadNext(mem, newPtr1);
    };

    case ExprTag.AssignRef: {
      return ptr + 8;
    };
    case ExprTag.Ref: {
      return ptr + 4;
    };
    case ExprTag.FreeScope: {
      return ptr + 4;
    };
    case ExprTag.MoveScope: {
      return ptr + 8;
    };

    case ExprTag.PeekStack: {
      return ptr + 1;
    };
    case ExprTag.PushStack: {
      return lookaheadNext(mem, ptr);
    };
    case ExprTag.PopStack: {
      return ptr;
    };

    case ExprTag.PopIns: {
      return ptr;
    };
    case ExprTag.ElementProp: {
      const newPtr0 = lookaheadNext(mem, ptr);
      const newPtr1 = lookaheadString(mem, newPtr0);
      return lookaheadNext(mem, newPtr1);
    };
    case ExprTag.ElementAttr: {
      const newPtr0 = lookaheadNext(mem, ptr);
      const newPtr1 = lookaheadString(mem, newPtr0);
      return lookaheadString(mem, newPtr1);
    };
    case ExprTag.ClassListAdd: {
      const newPtr0 = lookaheadNext(mem, ptr);
      return lookaheadString(mem, newPtr0);
    };
    case ExprTag.ClassListRemove: {
      const newPtr0 = lookaheadNext(mem, ptr);
      return lookaheadString(mem, newPtr0);
    };
    case ExprTag.InsertPlaceholder: {
      return ptr;
    };
    case ExprTag.InsertPlaceholderBefore: {
      return lookaheadNext(mem, ptr);
    };
    case ExprTag.ClearPlaceholder: {
      return lookaheadNext(mem, ptr);
    };
    case ExprTag.DetachPlaceholder: {
      return lookaheadNext(mem, ptr);
    };
    case ExprTag.CreateElement: {
      return lookaheadString(mem, ptr);
    };
    case ExprTag.CreateElementNS: {
      const newPtr0 = lookaheadString(mem, ptr);
      return lookaheadString(mem, newPtr0);
    };
    case ExprTag.CreateText: {
      return lookaheadString(mem, ptr);
    };
    case ExprTag.UpdateText: {
      const newPtr0 = lookaheadNext(mem, ptr);
      return lookaheadString(mem, newPtr0);
    };
    case ExprTag.Eval: {
      return lookaheadString(mem, ptr);
    };
    case ExprTag.TriggerEvent: {
      return lookaheadNext(mem, ptr + 4);
    };
    case ExprTag.Resume: {
      return ptr + 4;
    };
  }
  throw new Error("lookAheadExpr: unimplemented");
}

export function lookaheadString(mem: DataView, ptr: Ptr): Ptr {
  const len = Number(mem.getBigUint64(ptr, false));
  return ptr + 8 + len;
}

export type EncoderState = {
  readonly mem: DataView;
  begin: Ptr;
  readonly end: Ptr;
};

export function encodeValue(self: EncoderState, val: unknown): void {
  if (typeof(val) === 'boolean') {
    self.mem.setUint8(self.begin, ExprTag.Bool);
    self.mem.setUint8(self.begin + 1, val ? 1 : 0);
    self.begin += 2;
    return;
  }
  if (typeof(val) === 'number') {
    if (Number.isInteger(val)) {
      self.mem.setUint8(self.begin, ExprTag.I32);
      self.mem.setUint32(self.begin + 1, val, false);
      self.begin += 5;
      return;
    } else {
      self.mem.setUint8(self.begin, ExprTag.F64);
      self.mem.setFloat64(self.begin + 1, val, true);
      self.begin += 9;
      return;
    }
  }
  if (typeof(val) === 'string') {
    self.mem.setUint8(self.begin, ExprTag.Str);
    self.begin++;
    encodeString(self, val);
    return;
  }
  if (typeof(val) === 'bigint') {
    self.mem.setUint8(self.begin, ExprTag.I64);
    self.mem.setBigInt64(self.begin + 1, BigInt(val), false);
    self.begin += 9;
    return;
  }
  if (Array.isArray(val)) {
    self.mem.setUint8(self.begin, ExprTag.Arr);
    self.mem.setBigUint64(self.begin + 1, BigInt(val.length), false);
    self.begin += 9;
    val.forEach(v => encodeValue(self, v));
    return;
  }
  if (val instanceof Uint8Array) {
    self.mem.setUint8(self.begin, ExprTag.U8Arr);
    self.mem.setBigUint64(self.begin + 1, BigInt(val.byteLength), false);
    new Uint8Array(self.mem.buffer).set(val, self.begin + 9);
    self.begin += 9 + val.byteLength;
    return;
  }
  if (val === null || val === undefined) {
    self.mem.setUint8(self.begin, ExprTag.Null);
    self.begin++;
    return;
  }

  const entries = Object.entries(val);
  self.mem.setUint8(self.begin, ExprTag.Obj);
  self.mem.setBigUint64(self.begin + 1, BigInt(entries.length), false);
  self.begin += 9;

  entries.forEach(([k, v]) => {
    encodeString(self, k);
    encodeValue(self, v);
  });
  return;
}

function encodeString(self: EncoderState, s: string): void {
  const destRange = new Uint8Array(self.mem.buffer).subarray(self.begin + 8);
  const encResult = new TextEncoder().encodeInto(s, destRange)
  self.mem.setBigUint64(self.begin, BigInt(encResult.written), false);
  self.begin += 8 + encResult.written;
  return;
}

export enum ClientMsgTag {
  StartMsg,
  EventMsg,
  ResumeMsg,
  BeforeUnloadMsg,
}

export type ClientMsg =
  | [ClientMsgTag.StartMsg, unknown]
  | [ClientMsgTag.EventMsg, number, unknown]
  | [ClientMsgTag.ResumeMsg, number, unknown]
  | [ClientMsgTag.BeforeUnloadMsg]
;

export function encodeClientMessage(self: EncoderState, val: ClientMsg) {
  switch (val[0]) {
    case ClientMsgTag.StartMsg: {
      self.mem.setUint8(self.begin, ClientMsgTag.StartMsg);
      self.begin++;
      encodeValue(self, val[1]);
      return;
    }
    case ClientMsgTag.EventMsg: {
      self.mem.setUint8(self.begin, ClientMsgTag.EventMsg);
      self.mem.setUint32(self.begin + 1, val[1], false);
      self.begin += 5;
      encodeValue(self, val[2]);
      return;
    }
    case ClientMsgTag.ResumeMsg: {
      self.mem.setUint8(self.begin, ClientMsgTag.ResumeMsg);
      self.mem.setUint32(self.begin + 1, val[1], false);
      self.begin += 5;
      encodeValue(self, val[2]);
      return;
    }
    case ClientMsgTag.BeforeUnloadMsg: {
      self.mem.setUint8(self.begin, ClientMsgTag.BeforeUnloadMsg);
      self.begin++;
      return;
    }
  }
}

const decoder = new TextDecoder('utf8');

function decodeString(s: EvalState): string {
  const len = Number(s.mem.getBigUint64(s.begin, false));
  s.begin += 8;
  const strView = new Uint8Array(s.mem.buffer).subarray(s.begin, s.begin + len);
  s.begin += len;
  return decoder.decode(strView);
}

namespace utils {
  export function insert(root: Element|Comment, child: Node): void {
    if (root instanceof Comment) {
      root.parentElement!.insertBefore(child, root);
    } else {
      root.appendChild(child);
    }
  }

  export function insertBefore(root: Element|Comment, child: Node, anchor: Element|Comment): void {
    const anchor_ = anchor instanceof Comment ? lookupOpen(anchor) : anchor;
    if (root instanceof Comment) {
      root.parentElement!.insertBefore(child, anchor_);
    } else {
      root.insertBefore(child, anchor_);
    }
  }

  export function prop(parent: Element|Comment, propName: string, propValue: unknown): void {
    if (parent instanceof Comment) {
      (parent.parentElement as any)[propName] = propValue;
    } else {
      (parent as any)[propName] = propValue;
    }
  }

  export function attr(root: Element|Comment, attrName: string, attrValue: string): void {
    const element = getBuilderElement(root);
    element.setAttribute(attrName, attrValue);
  }

  export function addEventListener(root: Element|Comment, eventName: string, listener: EventListener): void {
    const element = getBuilderElement(root);
    element.addEventListener(eventName, listener);
  }

  export function removeEventListener(root: Element|Comment, eventName: string, listener: EventListener): void {
    const element = getBuilderElement(root);
    element.removeEventListener(eventName, listener);
  }

  export function insertPlaceholder(root: Element|Comment): Comment {
    const begin = document.createComment('Placeholder {{{');
    const end = document.createComment('}}}');
    insert(root, begin);
    insert(root, end);
    return end;
  }

  export function insertPlaceholderBefore(root: Element|Comment, anchor: Element|Comment): Comment {
    const begin = document.createComment('Placeholder {{{');
    const end = document.createComment('}}}');
    insertBefore(root, begin, anchor);
    insertBefore(root, end, anchor);
    return end;
  }

  export function clearPlaceholder(placeholder: Comment|Element, detach: boolean): void {
    if (placeholder instanceof Comment) {
      let nestedCounter = 0;
      for (;;){
        if (!placeholder.previousSibling ||
          (nestedCounter == 0 && isOpenPlaceholder(placeholder.previousSibling))
          ) break;
        if (isClosingPlaceholder(placeholder.previousSibling)) nestedCounter++;
        else if (isOpenPlaceholder(placeholder.previousSibling)) nestedCounter--;
        placeholder.previousSibling!.parentNode!.removeChild(placeholder.previousSibling!);
      }
      if (detach) {
        placeholder.previousSibling!.parentNode!.removeChild(placeholder.previousSibling!);
        placeholder.parentNode!.removeChild(placeholder);
      }
    } else {
      placeholder.innerHTML = '';
    }
  }

  export function getBuilderElement(root: Element|Comment): Element {
    if (root instanceof Comment) {
      return root.parentElement!;
    }
    return root;
  }

  function isOpenPlaceholder(node: Node): boolean {
    if (node instanceof Comment && node.textContent == 'Placeholder {{{') {
      return true;
    }
    return false;
  }

  function isClosingPlaceholder(node: Node): boolean {
    if (node instanceof Comment && node.textContent == '}}}') {
      return true;
    }
    return false;
  }

  function lookupOpen(anchor: Node): Node|null {
    let nestedCounter = 0;
    for (;;) {
      if (!anchor.previousSibling ||
        (nestedCounter == 0 && isOpenPlaceholder(anchor.previousSibling))
      ) return anchor.previousSibling;
      if (isClosingPlaceholder(anchor.previousSibling)) nestedCounter++;
      else if (isOpenPlaceholder(anchor.previousSibling)) nestedCounter--;
      anchor = anchor.previousSibling!;
    }
  }
};

export class RefStore {
  constructor(
    public scopes = new Map<ScopeId, Set<RefId>>,
    public refs = new Map<RefId, unknown>,
  ) {}

  assignRef(scope: ScopeId, refId: RefId, value: unknown) {
    const refs0 = this.scopes.get(scope);
    const refs1 = refs0 || new Set();
    if (!refs0) this.scopes.set(scope, refs1);
    refs1.add(refId);
    this.refs.set(refId, value);
  }

  moveScope(src: ScopeId, dest: ScopeId) {
    const srcRefs = this.scopes.get(src);
    const destRefs = this.scopes.get(dest);
    if (!srcRefs || !destRefs) return;
    srcRefs.forEach(s => destRefs.add(s));
    this.scopes.delete(src);
  }

  freeScope(scope: ScopeId) {
    const refs = this.scopes.get(scope);
    if (!refs) return;
    refs.forEach(s => this.refs.delete(s));
    this.scopes.delete(scope);
  }
}
