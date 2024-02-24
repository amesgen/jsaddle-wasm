# jsaddle-wasm

Run [JSaddle][] `JSM` actions with the [GHC WASM backend][].

This can for example be used to compile and run [Miso][] or [Reflex][] apps in the browser.

> [!IMPORTANT]
> This project is in an early stage.

## Examples

### Miso

Several Miso examples: https://github.com/tweag/ghc-wasm-miso-examples

## How to use

Install a WASM-enabled GHC with support for the WASM JSFFI from [ghc-wasm-meta][] (GHC master as of 2024-02, but GHC 9.10 will also work once released).

Assuming you built your application as an `app :: JSM ()`:

```haskell
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run app
```

Build the WASM binary with the following GHC options:
```cabal
ghc-options: -no-hs-main -optl-mexec-model=reactor "-optl-Wl,--export=hs_start"
```

Now, run the post-linker script as described in the [GHC User's Guide][ghc-users-guide-js-api]; we will call the resulting JavaScript file `ghc_wasm_jsffi.js`.

Then, following the [GHC User's Guide][ghc-users-guide-js-api], you can run the WASM binary in the browser via e.g. [browser_wasi_shim][]:
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

## Potential future work

 - Take a closer look at synchronous callbacks (no special handling currently, but basic things like `stopPropagation` already seem to work fine).
 - Testing (e.g. via Selenium).
 - Add logging/stats.
 - Performance/benchmarking (not clear that this is actually a bottleneck for most applications).
    - Optimize existing command-based implementation.
       - Reuse buffers
       - Use a serialization format more efficient than JSON.
    - Skip JSaddle commands, use WASM JSFFI more directly. Not clear if this is worth the extra complexity.

## Related projects

 - [WebGHC/jsaddle-wasm](https://github.com/WebGHC/jsaddle-wasm) for the analogue for [WebGHC][] instead of the [GHC WASM backend][].

[JSaddle]: https://github.com/ghcjs/jsaddle
[GHC WASM backend]: https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc
[Miso]: https://github.com/dmjio/miso
[Reflex]: https://github.com/reflex-frp/reflex
[ghc-wasm-meta]: https://gitlab.haskell.org/ghc/ghc-wasm-meta
[browser_wasi_shim]: https://github.com/bjorn3/browser_wasi_shim
[ghc-users-guide-js-api]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#the-javascript-api
[WebGHC]: https://webghc.github.io
