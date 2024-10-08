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
  FreeRef,
  Ref,
  FreeScope,

  PeekStack,
  PushStack,
  PopStack,

  InsPop,
  ElementProp,
  ElementAttr,
  ClassListAdd,
  ClassListRemove,
  InsertBrackets,
  ClearBrackets,
  DropBrackets,

  CreateElement,
  CreateElementNS,
  CreateTextNode,
  UpdateTextNode,

  Eval,
  TriggerEvent,
  YieldResult,
}

/** Encode `Expr` sum type as a union of disjoint n-tuples, keep in
 * sync with `Expr` definition in
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
  | [ExprTag.Apply, Expr, Expr]
  | [ExprTag.Call, Expr, string, Expr]

  | [ExprTag.AssignRef, number, number, Expr]
  | [ExprTag.FreeRef, number, number]
  | [ExprTag.Ref, number, number]
  | [ExprTag.FreeScope, number]

  | [ExprTag.PeekStack, number]
  | [ExprTag.PushStack, Expr]
  | [ExprTag.PopStack]

  | [ExprTag.InsPop, Expr, Expr]
  | [ExprTag.ElementProp, string, Expr]
  | [ExprTag.ElementAttr, string, string]
  | [ExprTag.ClassListAdd, string[]]
  | [ExprTag.ClassListRemove, string[]]
  | [ExprTag.InsertBrackets]
  | [ExprTag.ClearBrackets, Expr]
  | [ExprTag.DropBrackets, Expr]

  | [ExprTag.CreateElement, string]
  | [ExprTag.CreateElementNS, string, string]
  | [ExprTag.CreateTextNode, string]
  | [ExprTag.UpdateTextNode, Expr, string]

  | [ExprTag.Eval, string]
  | [ExprTag.TriggerEvent, number, Expr]
  | [ExprTag.YieldResult, number]
  ;

export type Ptr = number;
export type ScopeId = number;
export type RefId = number;
export type EventId = number;

export type EvalState = {
  readonly triggerEvent: (eventId: EventId, arg: unknown) => void;
  readonly resumeCont: (contId: number, res: unknown) => void;
  readonly persistent: PersistentState;
  readonly mem: DataView;
  readonly isMutableMem: boolean;
  begin: Ptr;
  readonly end: Ptr;
};

export type PersistentState = {
  readonly refs: Map<ScopeId, Map<RefId, unknown>>;
  stack: List<unknown>;
};

/** Evaluate sequence of `Expr` encoded as `Data.Binary.Binary`
 * instance inside the memory given in `self.mem` */
export function evalMem(self: EvalState): void {
  let res = null;
  for (; self.begin < self.end;) {
    res = evalNext(self, null, res);
  }
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
      const ptrEnd = lookAheadExpr(self.mem, self.begin);
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
      const fn = evalNext(self, args, prevRes);
      const arg = evalNext(self, args, prevRes);
      return (fn as any)(arg);
    };
    case ExprTag.Call: {
      const obj = evalNext(self, args, prevRes);
      const key = decodeString(self);
      const arg = evalNext(self, args, prevRes);
      return (obj as any)[key].call(obj, arg);
    };

    case ExprTag.AssignRef: {
      const scopeId = self.mem.getUint32(self.begin, false);
      const refId = self.mem.getUint32(self.begin + 4, false);
      self.begin += 8;
      const val = evalNext(self, args, prevRes);
      if (self.persistent.refs.has(scopeId)) {
        const scopeMap = self.persistent.refs.get(scopeId)!;
        scopeMap.set(refId, val);
      } else {
        const scopeMap = new Map();
        scopeMap.set(refId, val);
        self.persistent.refs.set(scopeId, scopeMap);
      }
      return val;
    };
    case ExprTag.FreeRef: {
      const scopeId = self.mem.getUint32(self.begin, false);
      const refId = self.mem.getUint32(self.begin + 4, false);
      self.begin += 8;
      const scopeMap = self.persistent.refs.get(scopeId);
      if (!scopeMap) return null;
      scopeMap.delete(refId);
      if (scopeMap.size == 0) {
        self.persistent.refs.delete(scopeId);
      }
      return null;
    };
    case ExprTag.Ref: {
      const scopeId = self.mem.getUint32(self.begin, false);
      const refId = self.mem.getUint32(self.begin + 4, false);
      self.begin += 8;
      return self.persistent.refs.get(scopeId)?.get(refId);
    };
    case ExprTag.FreeScope: {
      const scopeId = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      self.persistent.refs.delete(scopeId);
      return null;
    };

    case ExprTag.PeekStack: {
      const stackIx = self.mem.getInt8(self.begin);
      self.begin += 1;
      let iter = self.persistent.stack;
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
      self.persistent.stack = Cons(val, self.persistent.stack);
      return val;
    };
    case ExprTag.PopStack: {
      if (!self.persistent.stack) {
        throw new Error("PopStack: empty stack");
      }
      const res = self.persistent.stack[VAL];
      self.persistent.stack = self.persistent.stack[NEXT];
      return res;
    };

    case ExprTag.InsPop: {
      if (!self.persistent.stack) {
        throw new Error("InsPop: empty stack");
      }
      const tip = self.persistent.stack[VAL] as Node;
      self.persistent.stack = self.persistent.stack[NEXT];
      if (!self.persistent.stack) {
        throw new Error("InsPop: empty stack");
      }
      const parent = self.persistent.stack[VAL] as Element;
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
      throw new Error("Unimplemented");
    };
    case ExprTag.ClassListRemove: {
      throw new Error("Unimplemented");
    };
    case ExprTag.InsertBrackets: {
      if (!self.persistent.stack) {
        throw new Error("InsertBrackets: empty stack");
      }
      const el = self.persistent.stack[VAL] as any;
      return utils.insertBrackets(el);
    };
    case ExprTag.ClearBrackets: {
      const node = evalNext(self, args, prevRes);
      utils.clearBrackets(node as any, false);
      return null;
    };
    case ExprTag.DropBrackets: {
      const node = evalNext(self, args, prevRes);
      utils.clearBrackets(node as any, true);
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
    case ExprTag.CreateTextNode: {
      const content = decodeString(self);
      return document.createTextNode(content);
    };
    case ExprTag.UpdateTextNode: {
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
      self.triggerEvent(eventId, pload);
      return null;
    };
    case ExprTag.YieldResult: {
      const contId = self.mem.getUint32(self.begin, false);
      self.begin += 4;
      self.resumeCont(contId, prevRes);
      return null;
    };
  }
}

/** Parse next Expr without evaluating it. This is needed to clone
 * body expression of the Lam
 */
export function lookAheadExpr(mem: DataView, ptr: Ptr): Ptr {
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
    // case ExprTag.Arr: {
    //   const len = Number(view.getBigUint64(ptr, false));
    //   const result = new Array(len).fill(undefined);
    //   let iter = ptr + 8;
    //   for (let i = 0; i < len; i++) {
    //     const [newIter, val] = interpBytes(self, mem, iter);
    //     result[i] = val;
    //     iter = newIter;
    //   }
    //   return [iter, result];
    // };
    // case ExprTag.Obj: {
    //   const len = Number(view.getBigUint64(ptr, false));
    //   const result: Record<string, unknown> = {};
    //   let iter = ptr + 8;

    //   for (let i = 0; i < len; i++) {
    //     const [newIter1, key] = decodeString(self, mem, iter);
    //     const [newIter2, val] = interpBytes(self, mem, newIter1);

    //     result[key] = val;
    //     iter = newIter2;
    //   }
    //   return [iter, result];
    // };
    // case ExprTag.U8Arr: {
    //   const len = Number(view.getBigUint64(ptr, false));
    //   const res = mem.subarray(ptr + 8, ptr + 8 + len);
    //   return [ptr + 8 + len, res];
    // };

    // case ExprTag.Dot: {
    //   const [newPtr1, obj] = interpBytes(self, mem, ptr);
    //   const [newPtr2, key] = decodeString(self, mem, newPtr1);
    //   return [newPtr2, (obj as any)[key]];
    // };
    // case ExprTag.SetProp: {
    //   const [newPtr1, obj] = interpBytes(self, mem, ptr);
    //   const [newPtr2, key] = decodeString(self, mem, newPtr1);
    //   const [newPtr3, val] = interpBytes(self, mem, newPtr2);
    //   (obj as any)[key] = val;
    //   return [newPtr3, null];
    // };
    // case ExprTag.Ix: {
    //   const [newPtr1, obj] = interpBytes(self, mem, ptr);
    //   const key = view.getUint32(newPtr1);
    //   return [newPtr1 + 4, (obj as any)[key]]
    // };
    // case ExprTag.Id: {
    //   const [newPtr, iden] = decodeString(self, mem, ptr);
    //   return [newPtr, (global as any)[iden]];
    // };

    // case ExprTag.Lam: {
    //   throw new Error("Unimplemented");
    // };
    // case ExprTag.Arg: {
    //   const _argIx = view.getUint8(ptr);
    //   throw new Error("Unimplemented");
    // };
    // case ExprTag.Apply: {
    //   const [newPtr1, fn] = interpBytes(self, mem, ptr);
    //   const [newPtr2, arg] = interpBytes(self, mem, newPtr1);
    //   return [newPtr2, (fn as any)(arg)];
    // };
    // case ExprTag.Call: {
    //   const [newPtr1, obj] = interpBytes(self, mem, ptr);
    //   const [newPtr2, key] = decodeString(self, mem, newPtr1);
    //   const [newPtr3, arg] = interpBytes(self, mem, newPtr2);
    //   return [newPtr3, (obj as any)[key].call(obj, arg)];
    // };

    // case ExprTag.AssignRef: {
    //   const scopeId = view.getUint32(ptr, true);
    //   const refId = view.getUint32(ptr + 4, true);
    //   const [newPtr, val] = interpBytes(self, mem, ptr + 8);
    //   if (self.refs.has(scopeId)) {
    //     const scopeMap = self.refs.get(scopeId)!;
    //     scopeMap.set(refId, val);
    //   } else {
    //     const scopeMap = new Map();
    //     scopeMap.set(refId, val);
    //     self.refs.set(scopeId, scopeMap);
    //   }
    //   return [newPtr, val];
    // };
    // case ExprTag.FreeRef: {
    //   const scopeId = view.getUint32(ptr, true);
    //   const refId = view.getUint32(ptr + 4, true);
    //   const scopeMap = self.refs.get(scopeId);
    //   if (!scopeMap) return [ptr + 8, null];
    //   scopeMap.delete(refId);
    //   if (scopeMap.size == 0) {
    //     self.refs.delete(scopeId);
    //   }
    //   return [ptr + 8, null];
    // };
    // case ExprTag.Ref: {
    //   const scopeId = view.getUint32(ptr, true);
    //   const refId = view.getUint32(ptr + 4, true);
    //   const val = self.refs.get(scopeId)?.get(refId);
    //   return [ptr + 8, val];
    // };
    // case ExprTag.FreeScope: {
    //   const scopeId = view.getUint32(ptr, true);
    //   self.refs.delete(scopeId);
    //   return [ptr + 4, null];
    // };

    // case ExprTag.PeekStack: {
    //   const stackIx = view.getInt8(ptr);
    //   let iter = self.stack;
    //   let i = 0;
    //   while (iter) {
    //     if (i == stackIx) {
    //       return [ptr + 1, iter[VAL]];
    //     }
    //     iter = iter[NEXT];
    //     i++;
    //   }
    //   throw new Error("PeekStack: index out of stack size");
    // };
    // case ExprTag.PushStack: {
    //   const [newPtr, val] = interpBytes(self, mem, ptr);
    //   self.stack = Cons(val, self.stack);
    //   return [newPtr, val];
    // };
    // case ExprTag.PopStack: {
    //   if (!self.stack) {
    //     throw new Error("PopStack: empty stack");
    //   }
    //   const val = self.stack[VAL];
    //   self.stack = self.stack[NEXT];
    //   return [ptr, val];
    // };

    // case ExprTag.InsPop: {
    //   if (!self.stack) {
    //     throw new Error("InsPop: empty stack");
    //   }
    //   const tip = self.stack[VAL] as Node;
    //   self.stack = self.stack[NEXT];
    //   if (!self.stack) {
    //     throw new Error("InsPop: empty stack");
    //   }
    //   const parent = self.stack[VAL] as Element;
    //   utils.insert(parent, tip);
    //   return [ptr, tip];
    // };
    // case ExprTag.ElementProp: {
    //   const [newPtr1, el] = interpBytes(self, mem, ptr);
    //   const [newPtr2, key] = decodeString(self, mem, newPtr1);
    //   const [newPtr3, val] = interpBytes(self, mem, newPtr2);
    //   utils.prop(el as any, key, val);
    //   return [newPtr3, null];
    // };
    // case ExprTag.ElementAttr: {
    //   const [newPtr1, el] = interpBytes(self, mem, ptr);
    //   const [newPtr2, key] = decodeString(self, mem, newPtr1);
    //   const [newPtr3, val] = decodeString(self, mem, newPtr2);
    //   utils.attr(el as any, key, val);
    //   return [newPtr3, null];
    // };
    // case ExprTag.ClassListAdd: {
    //   throw new Error("Unimplemented");
    // };
    // case ExprTag.ClassListRemove: {
    //   throw new Error("Unimplemented");
    // };
    // case ExprTag.InsertBrackets: {
    //   if (!self.stack) {
    //     throw new Error("InsertBrackets: empty stack");
    //   }
    //   const el = self.stack[VAL] as any;
    //   const node = utils.insertBrackets(el);
    //   return [ptr, node];
    // };
    // case ExprTag.ClearBrackets: {
    //   const [newPtr, node] = interpBytes(self, mem, ptr);
    //   utils.clearBrackets(node as any, false);
    //   return [newPtr, null];
    // };
    // case ExprTag.DropBrackets: {
    //   const [newPtr, node] = interpBytes(self, mem, ptr);
    //   utils.clearBrackets(node as any, true);
    //   return [newPtr, null];
    // };
    // case ExprTag.CreateElement: {
    //   const [newPtr, tagName] = decodeString(self, mem, ptr);
    //   const el = document.createElement(tagName);
    //   return [newPtr, el];
    // };
    // case ExprTag.CreateElementNS: {
    //   const [newPtr1, tagName] = decodeString(self, mem, ptr);
    //   const [newPtr2, ns] = decodeString(self, mem, newPtr1);
    //   const el = document.createElementNS(tagName, ns);
    //   return [newPtr2, el];
    // };
    // case ExprTag.CreateTextNode: {
    //   const [newPtr, content] = decodeString(self, mem, ptr);
    //   const node = document.createTextNode(content);
    //   return [newPtr, node];
    // };
    // case ExprTag.UpdateTextNode: {
    //   const [newPtr1, node] = interpBytes(self, mem, ptr);
    //   const [newPtr2, content] = decodeString(self, mem, newPtr1);
    //   (node as Text).textContent = content;
    //   return [newPtr2, null];
    // };
    case ExprTag.Eval: {
      const len = Number(mem.getBigUint64(ptr, false));
      return ptr + 8 + len;
    };
    case ExprTag.TriggerEvent: {
      return lookAheadExpr(mem, ptr + 4);
    };
    case ExprTag.YieldResult: {
      return ptr + 4;
    };
  }
  throw new Error("lookAheadExpr: unimplemented");
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
    const destRange = new Uint8Array(self.mem.buffer).subarray(self.begin + 9);
    const encResult = new TextEncoder().encodeInto(val, destRange)
    self.mem.setBigUint64(self.begin + 1, BigInt(encResult.written), false);
    self.begin += 9 + encResult.written;
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
    encodeValue(self, k);
    encodeValue(self, v);
  });
  return;
}

export enum ClientMsgTag {
  StartMsg,
  EventMsg,
  ResumeMsg,
}

export type ClientMsg =
  | [ClientMsgTag.StartMsg, unknown]
  | [ClientMsgTag.EventMsg, number, unknown]
  | [ClientMsgTag.ResumeMsg, number, unknown]
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
  export function insert(builder: Element|Comment, child: Node): void {
    if (builder instanceof Comment) {
      builder.parentElement!.insertBefore(child, builder);
    } else {
      builder.appendChild(child);
    }
  }

  export function prop(parent: Element|Comment, propName: string, propValue: unknown): void {
    if (parent instanceof Comment) {
      (parent.parentElement as any)[propName] = propValue;
    } else {
      (parent as any)[propName] = propValue;
    }
  }

  export function attr(builder: Element|Comment, attrName: string, attrValue: string): void {
    const element = getBuilderElement(builder);
    element.setAttribute(attrName, attrValue);
  }

  export function addEventListener(builder: Element|Comment, eventName: string, listener: EventListener): void {
    const element = getBuilderElement(builder);
    element.addEventListener(eventName, listener);
  }

  export function removeEventListener(builder: Element|Comment, eventName: string, listener: EventListener): void {
    const element = getBuilderElement(builder);
    element.removeEventListener(eventName, listener);
  }

  export function insertBrackets(builder: Element|Comment): Comment {
    const begin = document.createComment('ContentBrackets {{');
    const end = document.createComment('}}');
    insert(builder, begin);
    insert(builder, end);
    return end;
  }

  export function clearBrackets(bracket: Comment|Element, detach: boolean): void {
    if (bracket instanceof Comment) {
      let nestedCounter = 0;
      for (;;){
        if (!bracket.previousSibling ||
          (nestedCounter == 0 && isOpenBracket(bracket.previousSibling))
          ) break;
        if (isClosingBracket(bracket.previousSibling)) nestedCounter++;
        else if (isOpenBracket(bracket.previousSibling)) nestedCounter--;
        bracket.previousSibling!.parentNode!.removeChild(bracket.previousSibling!);
      }
      if (detach) {
        bracket.previousSibling!.parentNode!.removeChild(bracket.previousSibling!);
        bracket.parentNode!.removeChild(bracket);
      }
    } else {
      bracket.innerHTML = '';
    }
  }

  export function getBuilderElement(builder: Element|Comment): Element {
    if (builder instanceof Comment) {
      return builder.parentElement!;
    }
    return builder;
  }

  function isOpenBracket(node: Node): boolean {
    if (node instanceof Comment && node.textContent == 'ContentBrackets {{') {
      return true;
    }
    return false;
  }

  function isClosingBracket(node: Node): boolean {
    if (node instanceof Comment && node.textContent == '}}') {
      return true;
    }
    return false;
  }
};
