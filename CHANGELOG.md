# Revision history for jsaddle-wasm

## 0.1.0.0 -- 2025-03-06

 * Add support for synchronous callbacks (when using `run`, but not `runWorker`) using the new synchronous Wasm JSFFI exports feature.
   Correspondingly, jsaddle-wasm requires a GHC with support for that feature; please use older versions of this library if you can not yet upgrade.

## 0.0.1.0 -- 2024-10-26

 * Added `runWorker` and `jsaddleScript` for running JSaddle and WASM logic in different execution environments. See the README for details on how to use this.

## 0.0.0.0 -- 2024-10-20

 * First version.
