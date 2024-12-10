# jsaddle-wasm
[![CI](https://github.com/amesgen/jsaddle-wasm/workflows/CI/badge.svg)](https://github.com/amesgen/jsaddle-wasm/actions)
[![Hackage](https://img.shields.io/hackage/v/jsaddle-wasm)](https://hackage.haskell.org/package/jsaddle-wasm)
[![Haddocks](https://img.shields.io/badge/documentation-Haddocks-purple)](https://hackage.haskell.org/package/jsaddle-wasm/docs/Language-Javascript-JSaddle-Wasm.html)

Run [JSaddle][] `JSM` actions with the [GHC Wasm backend][].

This can for example be used to compile and run [Miso][] or [Reflex][] apps in the browser.

> [!IMPORTANT]
> This project is in an early stage.

## Examples

 - Miso examples: https://github.com/tweag/ghc-wasm-miso-examples

 - Reflex examples: https://github.com/tweag/ghc-wasm-reflex-examples

 - Ormolu Live: https://github.com/tweag/ormolu/tree/master/ormolu-live
   (uses the web worker approach described below)

## How to use

Install a Wasm-enabled GHC with support for the Wasm JSFFI from [ghc-wasm-meta][] (GHC 9.10 or newer).

Assuming you built your application as an `app :: JSM ()`:

```haskell
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run app
```

Build the Wasm binary with the following GHC options:
```cabal
ghc-options: -no-hs-main -optl-mexec-model=reactor "-optl-Wl,--export=hs_start"
```

Now, run the post-linker script as described in the [GHC User's Guide][ghc-users-guide-js-api]; we will call the resulting JavaScript file `ghc_wasm_jsffi.js`.

Then, following the [GHC User's Guide][ghc-users-guide-js-api], you can run the Wasm binary in the browser via e.g. [browser_wasi_shim][]:
```javascript
import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
const options = { debug: false };
const wasi = new WASI([], [], fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();
```

### Separating execution environments

It is also possible to run the Wasm worker in a different execution environment (e.g. a web worker) than the JSaddle JavaScript code that dispatches the JSaddle command messages.

An advantage of this approach is that computationally expensive operations in Wasm do not block the UI thread. A disadvantage is that there is some overhead for copying the data back and forth, and everything relying on synchronous callbacks (e.g. `stopPropagation`/`preventDefault`) definitely no longer works.

 - Instead of the `run` function above, you need to use `runWorker` (again assuming `app :: JSM ()`):

   ```haskell
   import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

   foreign export javascript "hs_runWorker" runWorker :: JSVal -> IO ()

   runWorker :: JSVal -> IO ()
   runWorker = JSaddle.Wasm.runWorker app
   ```

   The argument to `runWorker` here can be any message port in the sense of the [Channel Messaging API][]. In particular, it must provide a `postMessage` function and a `message` event.

   For example, in a web worker, you can initialize the Wasm module as above, and then run
   ```javascript
   await instance.exports.hs_runWorker(globalThis);
   ```
   as `globalThis` (or `self`) in a web worker is a message port.

 - Additionally, you need to run the JSaddle command dispatching logic on the other end of the message port.

   The necessary chunk of JavaScript is available as `jsaddleScript` both from `Language.Javascript.JSaddle.Wasm` from the main library, and also from `Language.Javascript.JSaddle.Wasm.JS` from the `js` public sublibrary, where the latter has the advantage to not depend on any JSFFI, so you can build a normal WASI command module or even a native executable while still depending on it.

   It provides a function `runJSaddle` taking a single argument, a message port.

   One way to invoke it is to save `jsaddleScript` to some file, include it via a `script` tag in your HTML file, and then run
   ```javascript
   const worker = new Worker("my-worker.js");
   runJSaddle(worker);
   ```

## Potential future work

 - Take a closer look at synchronous callbacks. We have no special handling currently, but basic things like `stopPropagation`/`preventDefault` already seem to work fine with `run` (but not with `runWorker`, as expected).
 - Testing (e.g. via Selenium).
 - Add logging/stats.
 - Performance/benchmarking (not clear that this is actually a bottleneck for most applications).
    - Optimize existing command-based implementation.
       - Reuse buffers
       - Use a serialization format more efficient than JSON.
    - Patch `jsaddle` to not go through commands, by using the Wasm JS FFI.
    - Implement `ghcjs-dom` API directly via the Wasm JS FFI.

      This would involve creating a `ghcjs-dom-wasm` package by adapting the FFI import syntax from `ghcjs-dom-jsffi`/`ghcjs-dom-javascript` appropriately.

      Currently, the generic `ghcjs-dom-jsaddle` seems to work fine, so it seems sensible to wait with this until benchmarks or other concerns motivate this.

## Related projects

 - [WebGHC/jsaddle-wasm](https://github.com/WebGHC/jsaddle-wasm) for the analogue for [WebGHC][] instead of the [GHC Wasm backend][].

[JSaddle]: https://github.com/ghcjs/jsaddle
[GHC Wasm backend]: https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc
[Miso]: https://github.com/dmjio/miso
[Reflex]: https://github.com/reflex-frp/reflex
[ghc-wasm-meta]: https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
[browser_wasi_shim]: https://github.com/bjorn3/browser_wasi_shim
[ghc-users-guide-js-api]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#the-javascript-api
[WebGHC]: https://webghc.github.io
[Channel Messaging API]: https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API
