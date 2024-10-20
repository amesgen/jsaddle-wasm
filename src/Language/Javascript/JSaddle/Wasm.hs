module Language.Javascript.JSaddle.Wasm (run) where

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Wasm.Internal qualified as Internal

-- | Run a 'JSM' action via the WASM JavaScript FFI.
--
-- See the [README](https://github.com/amesgen/jsaddle-wasm) for more details.
--
-- While this package also compiles on non-WASM GHCs for convenience, running
-- this function will immediately fail.
run :: JSM () -> IO ()
run = Internal.run
