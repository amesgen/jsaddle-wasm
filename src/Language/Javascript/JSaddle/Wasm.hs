-- | See the [README](https://github.com/amesgen/jsaddle-wasm) for more details.
--
-- While this package also compiles on non-Wasm GHCs for convenience, running
-- any function from this module will immediately fail in that case.
module Language.Javascript.JSaddle.Wasm
  ( run,
    runWorker,

    -- * Re-exports
    jsaddleScript,
    JSVal,
  )
where

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Wasm.Internal (JSVal)
import Language.Javascript.JSaddle.Wasm.Internal qualified as Internal
import Language.Javascript.JSaddle.Wasm.JS (jsaddleScript)

-- | Run a 'JSM' action via the Wasm JavaScript FFI.
run :: JSM () -> IO ()
run = Internal.run

-- | Run the "worker part" of a 'JSM' action, interacting with the JSaddle JS
-- code via the given 'JSVal', a message port like e.g. a web worker.
--
-- The messages at the connected message port must be dispatched using
-- 'jsaddleScript'.
runWorker :: JSM () -> JSVal -> IO ()
runWorker = Internal.runWorker
