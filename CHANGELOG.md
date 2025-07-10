# Revision history for jsaddle-wasm

## 0.1.2.1 -- 2025-07-10

 * `Language.Javascript.JSaddle.Wasm.TH`: Make sure that the underlying FFI call has been fully completed.

## 0.1.2.0 -- 2025-06-27

 * Internally, stop using JS `eval`. This allows usage with a `Content-Security-Policy` without `unsafe-eval` (but still with `wasm-unsafe-eval`).

   For the same reason, expose `eval` and `evalFile` from `Language.Javascript.JSaddle.Wasm.TH` which allow to generate corresponding Wasm JSFFI imports for statically known strings.

   Useful as a replacement of JSaddle's `eval` in downstream libraries

## 0.1.1.0 -- 2025-05-01

 * Bug fix: make GHCJS helpers globally available.

   This bug manifested itself in error messages like
   ```
   TypeError: jsaddle_values.get(...) is undefined
   ```
   when calling certain JSaddle functions like `jsTypeOf` or `createFromArrayBuffer`.

## 0.1.0.0 -- 2025-03-06

 * Add support for synchronous callbacks (when using `run`, but not `runWorker`) using the new synchronous Wasm JSFFI exports feature.
   Correspondingly, jsaddle-wasm requires a GHC with support for that feature; please use older versions of this library if you can not yet upgrade.

## 0.0.1.0 -- 2024-10-26

 * Added `runWorker` and `jsaddleScript` for running JSaddle and WASM logic in different execution environments. See the README for details on how to use this.

## 0.0.0.0 -- 2024-10-20

 * First version.
