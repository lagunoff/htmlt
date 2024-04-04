import { evalMessage } from './ffi';

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
