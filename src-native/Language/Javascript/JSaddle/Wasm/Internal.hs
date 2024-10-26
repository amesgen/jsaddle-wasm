module Language.Javascript.JSaddle.Wasm.Internal
  ( run,
    runWorker,
    JSVal,
  )
where

import Language.Javascript.JSaddle.Types (JSM)

run :: JSM () -> IO ()
run _ =
  fail "Language.Javascript.JSaddle.Wasm.run: only works on WASM backend"

runWorker :: JSM () -> JSVal -> IO ()
runWorker _ _ =
  fail "Language.Javascript.JSaddle.Wasm.runWorker: only works on WASM backend"

data JSVal
