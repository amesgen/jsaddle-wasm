module Language.Javascript.JSaddle.Wasm.Internal (run) where

import Language.Javascript.JSaddle.Types (JSM)

run :: JSM () -> IO ()
run _ = fail "Language.Javascript.JSaddle.Wasm.run: only works on WASM backend"
