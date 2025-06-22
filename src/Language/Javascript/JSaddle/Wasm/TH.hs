-- | Utilities for evaluating (at runtime) JS code that is known at compile time
-- via TemplateHaskell /without/ relying on JS @eval@.
--
-- This functionality is not actually depending on JSaddle in any way; it is
-- just exposed here because it is also needed internally.
--
-- While this module also compiles on non-Wasm GHCs for convenience, the
-- TemplateHaskell helpers will generate functions that fail at runtime in that
-- case.
module Language.Javascript.JSaddle.Wasm.TH where

import Language.Haskell.TH qualified as TH
import Language.Javascript.JSaddle.Wasm.Internal.TH qualified as Internal

-- | Generate an expression that, when called, evaluates the given chunk of JS
-- code. Additionally, a list of argument types can be specified.
--
-- For example,
--
-- > $(eval "console.log('hi')" []) :: IO ()
--
-- will print \"hi\" to the console when executed.
--
-- Internally, this generates an appropriate safe Wasm JSFFI import.
--
-- Additionally, one can pass a list of types that will be used as arguments,
-- which can be referenced via \"$1\",\"$2\", etc. like in a regular Wasm JSFFI
-- import.
eval :: String -> [TH.Q TH.Type] -> TH.Q TH.Exp
eval = Internal.eval

-- | Like 'eval', but read the JS code to evaluate from a file.
evalFile :: FilePath -> TH.Q TH.Exp
evalFile path = do
  chunk <- TH.runIO $ readFile path
  eval chunk []
