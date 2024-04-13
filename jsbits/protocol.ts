import * as b from './binary';
import { Maybe, MaybeTag } from './binary';
import { absurd, IntMap } from './lib';

export type JSFunctionName = string;

export type Ident = string;

export type Bindings = Record<Ident, unknown>;

export type List<T> = null | Cons<T>;

export type Cons<T> = { 0: T, 1: List<T> };

export function Cons<T>(x: T, xs: List<T>): List<T> {
  return [x, xs];
}

export function car<T>(pair: Cons<T>): T {
  return pair[0];
}

export function cdr<T>(pair: Cons<T>): List<T> {
  return pair[1];
}

export type HaskellCallback = (jsMsg: JavaScriptMessage, argScope: List<IArguments>) => void;

export function evalExpr(hscb: HaskellCallback, idenScope: List<Bindings>, argScope: List<IArguments>, exp: Expr): unknown {
  switch(exp.tag) {
    case ExprTag.Null: {
       return null;
    }
    case ExprTag.Boolean: {
       return exp[0] != 0;
    }
    case ExprTag.I32: {
      return exp[0];
    }
    case ExprTag.F64: {
      return exp[0];
    }
    case ExprTag.String: {
      return exp[0];
    }
    case ExprTag.Array: {
      return exp[0].map(evalExpr.bind(undefined, hscb, idenScope, argScope));
    }
    case ExprTag.Object: {
      return Object.fromEntries(exp[0].map(([k, e]) => [k, evalExpr(hscb, idenScope, argScope, e)]));
    }
    case ExprTag.Uint8Array: {
      return exp[0];
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.SetProp: {
      const rhs = evalExpr(hscb, idenScope, argScope, exp[2]);
      const obj = evalExpr(hscb, idenScope, argScope, exp[0]) as any;
      obj[exp[1]] = rhs;
      return rhs;
    }
    case ExprTag.Ix: {
      const rhs: any = evalExpr(hscb, idenScope, argScope, exp.exp);
      return rhs[exp.ix];
    }
    case ExprTag.Plus: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as number;
      const rhs = evalExpr(hscb, idenScope, argScope, exp[1]) as number;
      return lhs + rhs;
    }
    case ExprTag.Subtract: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as number;
      const rhs = evalExpr(hscb, idenScope, argScope, exp[1]) as number;
      return lhs - rhs;
    }
    case ExprTag.Multiply: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as number;
      const rhs = evalExpr(hscb, idenScope, argScope, exp[1]) as number;
      return lhs * rhs;
    }
    case ExprTag.Divide: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as number;
      const rhs = evalExpr(hscb, idenScope, argScope, exp[1]) as number;
      return lhs / rhs;
    }
    case ExprTag.Id: {
      const ident = exp[0];
      for (let iter = idenScope; iter; iter = cdr(iter)) {
        const bindings = car(iter);
        if (ident in bindings) {
          // Found bound value
          return bindings[ident];
        }
      }
      throw new Error('Variable not in scope: ' + exp[0]);
    }
    case ExprTag.Lam: {
      return function() {
        return evalExpr(hscb, idenScope, Cons(arguments, argScope), exp.body);
      };
    }
    case ExprTag.Arg: {
      let iter = argScope;
      let j = 0;
      while (iter) {
        if (j == exp.scopeIx) {
          const iarguments = car(iter);
          return iarguments[exp.argIx];
        }
        iter = cdr(iter);
        j++;
      }
      throw new Error('Argument scope out of a range: ' + exp.scopeIx);
    }
    case ExprTag.Apply: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as Function;
      return lhs.apply(undefined, exp[1].map(evalExpr.bind(undefined, hscb, idenScope, argScope)));
    }
    case ExprTag.Call: {
      const lhs = evalExpr(hscb, idenScope, argScope, exp[0]) as any;
      const fn = lhs[exp[1]];
      return fn.apply(lhs, exp[2].map(evalExpr.bind(undefined, hscb, idenScope, argScope)));
    }
    case ExprTag.AssignVar: {
      const rhs = evalExpr(hscb, idenScope, argScope, exp.rhs);
      if (varStorage.has(exp.scopeId)) {
        const scopeMap = varStorage.get(exp.scopeId)!;
        scopeMap.set(exp.varId, rhs);
      } else {
        const scopeMap = new Map();
        scopeMap.set(exp.varId, rhs);
        varStorage.set(exp.scopeId, scopeMap);
      }
      return rhs;
    }
    case ExprTag.FreeVar: {
      const scopeStorage = varStorage.get(exp.scopeId);
      if (!scopeStorage) return;
      scopeStorage.delete(exp.varId);
      if (scopeStorage.size == 0) {
        varStorage.delete(exp.scopeId);
      }
      return;
    }
    case ExprTag.Var: {
      return varStorage.get(exp.scopeId)?.get(exp.varId);
    }
    case ExprTag.FreeScope: {
      varStorage.delete(exp.scopeId);
      const scopeFinalizers = finalizers.get(exp.scopeId);
      if (scopeFinalizers) scopeFinalizers.forEach(fn => fn ());
      return null;
    }
    case ExprTag.InsertNode: {
      const parent = evalExpr(hscb, idenScope, argScope, exp.parent) as Element|Comment;
      const child = evalExpr(hscb, idenScope, argScope, exp.child) as Node;
      domHelpers.insertIntoBuilder(parent, child);
      return null;
    }
    case ExprTag.CreateElement: {
      return document.createElement(exp.tagName);
    }
    case ExprTag.CreateElementNS: {
      return document.createElementNS(exp.ns, exp.tagName);
    }
    case ExprTag.CreateText: {
      return document.createTextNode(exp.content);
    }
    case ExprTag.ElementProp: {
      const parent = evalExpr(hscb, idenScope, argScope, exp.node) as Element|Comment;
      const propValue = evalExpr(hscb, idenScope, argScope, exp.propValue);
      domHelpers.assignProperty(parent, exp.propName, propValue);
      return null;
    }
    case ExprTag.ElementAttr: {
      const parent = evalExpr(hscb, idenScope, argScope, exp.node) as Element|Comment;
      domHelpers.assignAttribute(parent, exp.attrName, exp.attrValue);
      return null;
    }
    case ExprTag.InsertClassList: {
      const parent = evalExpr(hscb, idenScope, argScope, exp.node) as Element|Comment;
      const element = domHelpers.domBuilderElement(parent);
      exp.classList.forEach(className => element.classList.add(className));
      return null;
    }
    case ExprTag.RemoveClassList: {
      const parent = evalExpr(hscb, idenScope, argScope, exp.node) as Element|Comment;
      const element = domHelpers.domBuilderElement(parent);
      exp.classList.forEach(className => element.classList.remove(className));
      return null;
    }
    case ExprTag.UpdateTextNode: {
      const node = evalExpr(hscb, idenScope, argScope, exp.node) as Text;
      node.textContent = exp.content;
      return null;
    }
    case ExprTag.InsertBoundary: {
      const parent = evalExpr(hscb, idenScope, argScope, exp.parent) as Element|Comment;
      return domHelpers.insertBoundary(parent);
    }
    case ExprTag.ClearBoundary: {
      const boundary = evalExpr(hscb, idenScope, argScope, exp.boundary) as Comment;
      return domHelpers.clearBoundary(boundary, Boolean(exp.detach));
    }
    case ExprTag.AddEventListener: {
      const target = evalExpr(hscb, idenScope, argScope, exp.target) as Element|Comment;
      const eventName = evalExpr(hscb, idenScope, argScope, exp.eventName) as string;
      const listener = evalExpr(hscb, idenScope, argScope, exp.listener) as EventListener;
      domHelpers.addEventListener(target, eventName, listener);
      const existingScope = finalizers.get(exp.reactiveScope);
      const scopeFinalizers = existingScope ? existingScope : new IntMap<Function>();
      if (!existingScope) finalizers.set(exp.reactiveScope, scopeFinalizers);
      return scopeFinalizers.push(() => domHelpers.removeEventListener(target, eventName, listener));
    }
    case ExprTag.ConnectResource: {
      const finalizer = evalExpr(hscb, idenScope, argScope, exp.aquire) as Function;
      const existingScope = finalizers.get(exp.reactiveScope);
      const scopeFinalizers = existingScope ? existingScope : new IntMap<Function>();
      if (!existingScope) finalizers.set(exp.reactiveScope, scopeFinalizers);
      return scopeFinalizers.push(finalizer);
    }
    case ExprTag.SetTimeout: {
      const callback = evalExpr(hscb, idenScope, argScope, exp.callback) as Function;
      const existingScope = finalizers.get(exp.reactiveScope);
      const scopeFinalizers = existingScope ? existingScope : new IntMap<Function>();
      if (!existingScope) finalizers.set(exp.reactiveScope, scopeFinalizers);
      let timeoutId: NodeJS.Timeout|null = null;
      const finalizerId = scopeFinalizers.push(() => timeoutId && clearTimeout(timeoutId));
      timeoutId = setTimeout(() => {
        scopeFinalizers.delete(finalizerId);
        timeoutId = null;
        callback();
      }, exp.timeout);
      return finalizerId;
    }
    case ExprTag.ApplyFinalizer: {
      const existingScope = finalizers.get(exp.reactiveScope);
      const finalizerId = evalExpr(hscb, idenScope, argScope, exp.finalizerId) as number;
      if (!existingScope) return false;
      const cancellerFn = existingScope.get(finalizerId);
      if (!cancellerFn) return false;
      existingScope.delete(finalizerId);
      cancellerFn();
      return true;
    }
    case ExprTag.RevSeq: {
      return exp.exprs.reduceRight<unknown>((_, e) => evalExpr(hscb, idenScope, argScope, e), null);
    }
    case ExprTag.Eval: {
      return eval(exp.rawJavaScript);
    }
    case ExprTag.TriggerCallback: {
      const arg = evalExpr(hscb, idenScope, argScope, exp.arg);
      const jsMsg: JavaScriptMessage = {
        tag: JavaScriptMessageTag.TriggerCallback,
        arg: unknownToValue(arg),
        callbackId: exp.callbackId,
      }
      return hscb(jsMsg, argScope);
    }
    case ExprTag.UncaughtException: {
      throw new Error(exp.message);
    }
  }
  absurd(exp);
}

export function unknownToValue(inp: unknown): Value {
  if (typeof(inp) === 'boolean') {
    return { tag: ValueTag.Boolean, 0: inp ? 1 : 0 };
  }
  if (typeof(inp) === 'number') {
    if (Number.isInteger(inp)) {
      return { tag: ValueTag.I32, 0: inp };
    } else {
      return { tag: ValueTag.F64, 0: inp };
    }
  }
  if (typeof(inp) === 'string') {
    return { tag: ValueTag.String, 0: inp };
  }
  if (Array.isArray(inp)) {
    return { tag: ValueTag.Array, 0: inp.map(unknownToValue) };
  }
  if (inp instanceof Uint8Array) {
    return { tag: ValueTag.Uint8Array, 0: inp };
  }
  if (inp === null || inp === undefined) {
    return { tag: ValueTag.Null };
  }
  const entries = Object.entries(inp)
    .map(([k, v]) => [k, unknownToValue(v)] as KV);

  return { tag: ValueTag.Object, 0: entries }

  type KV = [string, Value];
}

export enum ValueTag {
  Null,
  Boolean,
  I32,
  F64,
  String,
  Array,
  Object,
  Uint8Array,
}

export type Value =
  | { tag: ValueTag.Null }
  | { tag: ValueTag.Boolean, 0: number }
  | { tag: ValueTag.I32, 0: number }
  | { tag: ValueTag.F64, 0: number }
  | { tag: ValueTag.String, 0: string }
  | { tag: ValueTag.Array, 0: Value[] }
  | { tag: ValueTag.Object, 0: [string, Value][] }
  | { tag: ValueTag.Uint8Array, 0: Uint8Array }
;

export const jvalue = b.recursive<Value>(self => b.discriminate({
  [ValueTag.Null]: b.record({ }),
  [ValueTag.Boolean]: b.record({ 0: b.int8 }),
  [ValueTag.I32]: b.record({ 0: b.int32 }),
  [ValueTag.F64]: b.record({ 0: b.float64 }),
  [ValueTag.String]: b.record({ 0: b.string }),
  [ValueTag.Array]: b.record({ 0: b.array(self) }),
  [ValueTag.Object]: b.record({ 0: b.array(b.tuple(b.string, self)) }),
  [ValueTag.Uint8Array]: b.record({ 0: b.u8array }),
}));

export enum ExprTag {
  Null,
  Boolean,
  I32,
  F64,
  String,
  Array,
  Object,
  Uint8Array,

  Dot,
  SetProp,
  Ix,

  Plus,
  Subtract,
  Multiply,
  Divide,

  Id,
  Lam,
  Arg,
  Apply,
  Call,

  AssignVar,
  FreeVar,
  Var,
  FreeScope,

  InsertNode,
  CreateElement,
  CreateElementNS,
  CreateText,
  ElementProp,
  ElementAttr,
  InsertClassList,
  RemoveClassList,
  UpdateTextNode,
  InsertBoundary,
  ClearBoundary,

  AddEventListener,
  ConnectResource,
  SetTimeout,
  ApplyFinalizer,

  RevSeq,
  Eval,
  TriggerCallback,
  UncaughtException,
}

export type Expr =
  | { tag: ExprTag.Null }
  | { tag: ExprTag.Boolean, 0: number }
  | { tag: ExprTag.I32, 0: number }
  | { tag: ExprTag.F64, 0: number }
  | { tag: ExprTag.String, 0: string }
  | { tag: ExprTag.Array, 0: Expr[] }
  | { tag: ExprTag.Object, 0: [string, Expr][] }
  | { tag: ExprTag.Uint8Array, 0: Uint8Array }

  | { tag: ExprTag.Dot, 0: Expr, 1: string }
  | { tag: ExprTag.SetProp, 0: Expr, 1: string, 2: Expr }
  | { tag: ExprTag.Ix, exp: Expr, ix: number }

  | { tag: ExprTag.Plus, 0: Expr, 1: Expr }
  | { tag: ExprTag.Subtract, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Multiply, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Divide, 0: Expr, 1: Expr  }

  | { tag: ExprTag.Id, 0: string }
  | { tag: ExprTag.Lam, body: Expr }
  | { tag: ExprTag.Arg, scopeIx: number, argIx: number }
  | { tag: ExprTag.Apply, 0: Expr, 1: Expr[] }
  | { tag: ExprTag.Call, 0: Expr, 1: JSFunctionName, 2: Expr[] }

  | { tag: ExprTag.AssignVar, scopeId: number, varId: number, rhs: Expr }
  | { tag: ExprTag.FreeVar, scopeId: number, varId: number }
  | { tag: ExprTag.Var, scopeId: number, varId: number }
  | { tag: ExprTag.FreeScope, scopeId: number }

  | { tag: ExprTag.InsertNode, parent: Expr, child: Expr }
  | { tag: ExprTag.CreateElement, tagName: string }
  | { tag: ExprTag.CreateElementNS, ns: string, tagName: string }
  | { tag: ExprTag.CreateText, content: string }
  | { tag: ExprTag.ElementProp, node: Expr, propName: string, propValue: Expr }
  | { tag: ExprTag.ElementAttr, node: Expr, attrName: string, attrValue: string }
  | { tag: ExprTag.InsertClassList, node: Expr, classList: string[] }
  | { tag: ExprTag.RemoveClassList, node: Expr, classList: string[] }
  | { tag: ExprTag.UpdateTextNode, node: Expr, content: string }
  | { tag: ExprTag.InsertBoundary, parent: Expr }
  | { tag: ExprTag.ClearBoundary, boundary: Expr, detach: number }

  | { tag: ExprTag.AddEventListener, reactiveScope: number, target: Expr, eventName: Expr, listener: Expr }
  | { tag: ExprTag.ConnectResource, reactiveScope: number, aquire: Expr }
  | { tag: ExprTag.SetTimeout, reactiveScope: number, callback: Expr, timeout: number }
  | { tag: ExprTag.ApplyFinalizer, reactiveScope: number, finalizerId: Expr }

  | { tag: ExprTag.RevSeq, exprs: Expr[] }
  | { tag: ExprTag.Eval, rawJavaScript: string }
  | { tag: ExprTag.TriggerCallback, callbackId: number, arg: Expr }
  | { tag: ExprTag.UncaughtException, message: string }
;

export const expr = b.recursive<Expr>(self => b.discriminate({
  [ExprTag.Null]: b.record({}),
  [ExprTag.Boolean]: b.record({ 0: b.int8 }),
  [ExprTag.I32]: b.record({ 0: b.int32 }),
  [ExprTag.F64]: b.record({ 0: b.float64 }),
  [ExprTag.String]: b.record({ 0: b.string }),
  [ExprTag.Array]: b.record({ 0: b.array(self) }),
  [ExprTag.Object]: b.record({ 0: b.array(b.tuple(b.string, self)) }),

  [ExprTag.Dot]: b.record({ 0: self, 1: b.string }),
  [ExprTag.SetProp]: b.record({ 0: self, 1: b.string, 2: self }),
  [ExprTag.Ix]: b.record({ exp: self, ix: b.int32 }),

  [ExprTag.Plus]: b.record({ 0: self, 1: self }),
  [ExprTag.Subtract]: b.record({ 0: self, 1: self }),
  [ExprTag.Multiply]: b.record({ 0: self, 1: self }),
  [ExprTag.Divide]: b.record({ 0: self, 1: self }),

  [ExprTag.Id]: b.record({ 0: b.string }),
  [ExprTag.Lam]: b.record({ body: self }),
  [ExprTag.Arg]: b.record({ scopeIx: b.int8, argIx: b.int8 }),
  [ExprTag.Apply]: b.record({ 0: self, 1: b.array(self) }),
  [ExprTag.Call]: b.record({ 0: self, 1: b.string, 2: b.array(self) }),

  [ExprTag.AssignVar]: b.record({ scopeId: b.int32, varId: b.int32, rhs: self }),
  [ExprTag.FreeVar]: b.record({ scopeId: b.int32, varId: b.int32 }),
  [ExprTag.Var]: b.record({ scopeId: b.int32, varId: b.int32 }),
  [ExprTag.FreeScope]: b.record({ scopeId: b.int32 }),

  [ExprTag.InsertNode]: b.record({ parent: self, child: self }),
  [ExprTag.CreateElement]: b.record({ tagName: b.string }),
  [ExprTag.CreateElementNS]: b.record({ ns: b.string, tagName: b.string }),
  [ExprTag.CreateText]: b.record({ content: b.string }),
  [ExprTag.ElementProp]: b.record({ node: self, propName: b.string, propValue: self }),
  [ExprTag.ElementAttr]: b.record({ node: self, attrName: b.string, attrValue: b.string }),
  [ExprTag.InsertClassList]: b.record({ node: self, classList: b.array(b.string) }),
  [ExprTag.RemoveClassList]: b.record({ node: self, classList: b.array(b.string) }),
  [ExprTag.UpdateTextNode]: b.record({ node: self, content: b.string }),
  [ExprTag.InsertBoundary]: b.record({ parent: self }),
  [ExprTag.ClearBoundary]: b.record({ boundary: self, detach: b.int8 }),

  [ExprTag.AddEventListener]: b.record({ reactiveScope: b.int32, target: self, eventName: self, listener: self }),
  [ExprTag.ConnectResource]: b.record({ reactiveScope: b.int32, aquire: self }),
  [ExprTag.SetTimeout]: b.record({ reactiveScope: b.int32, callback: self, timeout: b.int32 }),
  [ExprTag.ApplyFinalizer]: b.record({ reactiveScope: b.int32, finalizerId: self }),

  [ExprTag.RevSeq]: b.record({ exprs: b.array(self) }),
  [ExprTag.Eval]: b.record({ rawJavaScript: b.string }),
  [ExprTag.TriggerCallback]: b.record({ callbackId: b.int32, arg: self }),
  [ExprTag.UncaughtException]: b.record({ message: b.string }),
}));

export enum HaskellMessageTag {
  EvalExpr,
  HotReload,
  Halt,
}

export const haskellMessage = b.discriminate({
  [HaskellMessageTag.EvalExpr]: b.record({ expr: expr }),
  [HaskellMessageTag.HotReload]: b.record({ }),
  [HaskellMessageTag.Halt]: b.record({ }),
});

export enum JavaScriptMessageTag {
  Start,
  Return,
  TriggerCallback,
  BeforeUnload,
}

export const javascriptMessage = b.discriminate({
  [JavaScriptMessageTag.Start]: b.record({ 0: jvalue }),
  [JavaScriptMessageTag.Return]: b.record({ 0: jvalue }),
  [JavaScriptMessageTag.TriggerCallback]: b.record({ arg: jvalue, callbackId: b.int32 }),
  [JavaScriptMessageTag.BeforeUnload]: b.record({}),
});

export type HaskellMessage = typeof haskellMessage['_A'];
export type JavaScriptMessage = typeof javascriptMessage['_A'];

export type ReactiveScope = number;
export type VarId = number;

export const varStorage = new Map<ReactiveScope, Map<VarId, unknown>>();
export const finalizers = new Map<ReactiveScope, IntMap<Function>>;

namespace domHelpers {
  export function insertIntoBuilder(builder: Element|Comment, child: Node): void {
    if (builder instanceof Comment) {
      builder.parentElement!.insertBefore(child, builder);
    } else {
      builder.appendChild(child);
    }
  }

  export function assignProperty(parent: Element|Comment, propName: string, propValue: unknown): void {
    if (parent instanceof Comment) {
      (parent.parentElement as any)[propName] = propValue;
    } else {
      (parent as any)[propName] = propValue;
    }
  }

  export function assignAttribute(builder: Element|Comment, attrName: string, attrValue: string): void {
    const element = domBuilderElement(builder);
    element.setAttribute(attrName, attrValue);
  }

  export function addEventListener(builder: Element|Comment, eventName: string, listener: EventListener): void {
    const element = domBuilderElement(builder);
    element.addEventListener(eventName, listener);
  }

  export function removeEventListener(builder: Element|Comment, eventName: string, listener: EventListener): void {
    const element = domBuilderElement(builder);
    element.removeEventListener(eventName, listener);
  }

  export function insertBoundary(builder: Element|Comment): Comment {
    const begin = document.createComment('ContentBoundary {{');
    const end = document.createComment('}}');
    insertIntoBuilder(builder, begin);
    insertIntoBuilder(builder, end);
    return end;
  }

  export function clearBoundary(boundary: Comment, detach: boolean): void {
    const end = boundary;
    let nestedCounter = 0;
    for (;;){
      if (!end.previousSibling ||
        (nestedCounter == 0 && isOpeningBoundary(end.previousSibling))
         ) break;
      if (isClosingBoundary(end.previousSibling)) nestedCounter++;
      else if (isOpeningBoundary(end.previousSibling)) nestedCounter--;
      end.previousSibling!.parentNode!.removeChild(end.previousSibling!);
    }
    if (detach) {
      end.previousSibling!.parentNode!.removeChild(end.previousSibling!);
      end.parentNode!.removeChild(end);
    }
  }

  export function domBuilderElement(builder: Element|Comment): Element {
    if (builder instanceof Comment) {
      return builder.parentElement!;
    }
    return builder;
  }

  function isOpeningBoundary(node: Node): boolean {
    if (node instanceof Comment && node.textContent == 'ContentBoundary {{') {
      return true;
    }
    return false;
  }

  function isClosingBoundary(node: Node): boolean {
    if (node instanceof Comment && node.textContent == '}}') {
      return true;
    }
    return false;
  }
};
