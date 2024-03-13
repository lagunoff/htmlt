// This file implements the JavaScript runtime logic for Haskell
// modules that use JSFFI. It is not an ESM module, but the template
// of one; the post-linker script will copy all contents into a new
// ESM module.

// Manage a mapping from unique 32-bit ids to actual JavaScript
// values.
class JSValManager {
  #lastk = 0;
  #kv = new Map();

  constructor() {}

  // Maybe just bump this.#lastk? For 64-bit ids that's sufficient,
  // but better safe than sorry in the 32-bit case.
  #allocKey() {
    let k = this.#lastk;
    while (true) {
      if (!this.#kv.has(k)) {
        this.#lastk = k;
        return k;
      }
      k = (k + 1) | 0;
    }
  }

  newJSVal(v) {
    const k = this.#allocKey();
    this.#kv.set(k, v);
    return k;
  }

  // A separate has() call to ensure we can store undefined as a value
  // too. Also, unconditionally check this since the check is cheap
  // anyway, if the check fails then there's a use-after-free to be
  // fixed.
  getJSVal(k) {
    if (!this.#kv.has(k)) {
      throw new WebAssembly.RuntimeError(`getJSVal(${k})`);
    }
    return this.#kv.get(k);
  }

  // Check for double free as well.
  freeJSVal(k) {
    if (!this.#kv.delete(k)) {
      throw new WebAssembly.RuntimeError(`freeJSVal(${k})`);
    }
  }
}

// A simple & fast setImmediate() implementation for browsers. It's
// not a drop-in replacement for node.js setImmediate() because:
// 1. There's no clearImmediate(), and setImmediate() doesn't return
//    anything
// 2. There's no guarantee that callbacks scheduled by setImmediate()
//    are executed in the same order (in fact it's the opposite lol),
//    but you are never supposed to rely on this assumption anyway
class SetImmediate {
  #fs = [];
  #mc = new MessageChannel();

  constructor() {
    this.#mc.port1.addEventListener("message", () => {
      this.#fs.pop()();
    });
    this.#mc.port1.start();
  }

  setImmediate(cb, ...args) {
    this.#fs.push(() => cb(...args));
    this.#mc.port2.postMessage(undefined);
  }
}

// The actual setImmediate() to be used. This is a ESM module top
// level binding and doesn't pollute the globalThis namespace.
let setImmediate;
if (globalThis.setImmediate) {
  // node.js, bun
  setImmediate = globalThis.setImmediate;
} else {
  /* try {
   *   // deno
   *   setImmediate = (await import("node:timers")).setImmediate;
   * } catch { */
    // browsers
    const sm = new SetImmediate();
    setImmediate = (cb, ...args) => sm.setImmediate(cb, ...args);
  /* } */
}

export default (__exports) => {
const __ghc_wasm_jsffi_jsval_manager = new JSValManager();
const __ghc_wasm_jsffi_finalization_registry = new FinalizationRegistry(sp => __exports.rts_freeStablePtr(sp));
return {
newJSVal: (v) => __ghc_wasm_jsffi_jsval_manager.newJSVal(v),
getJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.getJSVal(k),
freeJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.freeJSVal(k),
scheduleWork: () => setImmediate(__exports.rts_schedulerLoop),
ZC0ZCmainZCUtilsZC: ($1,$2,$3,$4,$5,$6) => ((function(k1, v1, k2, v2, k3, v3){    var res = {};    res[k1] = v1; res[k2] = v2; res[k3] = v3;    return res;  })($1,$2,$3,$4,$5,$6)),
ZC1ZCmainZCUtilsZC: () => (location.hash),
ZC2ZCmainZCUtilsZC: ($1) => {var itemText = localStorage.getItem($1);   return itemText ? JSON.parse(itemText) : null;},
ZC3ZCmainZCUtilsZC: ($1) => (console.log($1)),
ZC4ZCmainZCUtilsZC: ($1,$2) => {localStorage.setItem($1, JSON.stringify($2));},
ZC5ZCmainZCUtilsZC: ($1) => (setTimeout(function() {    var inputEl = $1.parentNode.parentNode.querySelector('input.edit');    inputEl.focus();  }, 0)),
ZC1ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: async ($1,$2,$3) => ($1.addEventListener($2, $3)),
ZC3ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1) => ((a1) => __exports.ghczuwasmzujsffiZC2ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC($1,a1)),
ZC4ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2,$3) => {$1[$2] = $3;},
ZC7ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2) => {if ($1.parentNode) $1.parentNode.removeChild($1);   if ($2.parentNode) $2.parentNode.removeChild($2);},
ZC8ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2) => {for (;;){    if (!$2.previousSibling      || !$2.previousSibling.parentNode      || $2.previousSibling === $1      ) break;    $2.previousSibling.parentNode.removeChild($2.previousSibling);  }},
ZC9ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: () => (window.document.body),
ZC11ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: () => {return window},
ZC12ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: async ($1) => (window.addEventListener('beforeunload', $1)),
ZC13ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2) => {$1.nodeValue = $2;},
ZC14ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2) => {$1.classList.remove($2);},
ZC15ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2) => {$1.classList.add($2);},
ZC16ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1) => (document.createComment($1)),
ZC17ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1) => (document.createTextNode($1)),
ZC19ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1) => (document.createElement($1)),
ZC23ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2,$3) => ($1.setAttribute($2, $3)),
ZC24ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2,$3) => ($1.insertBefore($2, $3)),
ZC25ZChtmltzm0zi1zi0zi0zminplaceZCHtmlTziDOMZC: ($1,$2) => ($1.appendChild($2)),
ZC0ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => ($1.length),
ZC1ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1,$2,$3) => ((new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written),
ZC2ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1,$2) => ((new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $1, $2)))),
ZC3ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1,$2) => ($1[$2]),
ZC4ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => ($1.length),
ZC5ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1,$2) => ($1.push($2)),
ZC6ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: () => ([]),
ZC8ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1,$2) => ($1[$2]),
ZC9ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => (($1 ? true : false)),
ZC11ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => ($1),
ZC12ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => ($1),
ZC13ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => {if ($1 === undefined || $1 === null) return 0;   if (typeof $1 === 'boolean') return 1;   if (typeof $1 === 'number') return 2;   if (typeof $1 === 'string') return 3;   if (Array.isArray($1)) return 4;   return 5;},
ZC14ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: ($1) => (($1 === null || $1 === undefined)),
ZC17ZChtmltzm0zi1zi0zi0zminplaceZCWasmziCompatziMarshalZC: () => (null),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => ($1.reject(new WebAssembly.RuntimeError($2))),
ZC19ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => ($1.resolve()),
ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: () => {let res, rej; const p = new Promise((resolve, reject) => { res = resolve; rej = reject; }); p.resolve = res; p.reject = rej; return p;},
ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => (__ghc_wasm_jsffi_finalization_registry.register($1, $2, $1)),
ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1,$2) => ($1.then(() => __exports.rts_promiseResolveUnit($2), err => __exports.rts_promiseReject($2, err))),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => (`${$1.stack ? $1.stack : $1}`),
ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1,$2) => ((new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))),
ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1,$2,$3) => ((new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written),
ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => ($1.length),
ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => {if (!__ghc_wasm_jsffi_finalization_registry.unregister($1)) { throw new WebAssembly.RuntimeError('js_callback_unregister'); }},
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC: async ($1) => (new Promise(res => setTimeout(res, $1 / 1000))),
};
};
