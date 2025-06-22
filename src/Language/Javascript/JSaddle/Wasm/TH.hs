{-# LANGUAGE CPP #-}

-- | Utilities for evaluating (at runtime) JS code that is known at compile time
-- via TemplateHaskell /without/ relying on JS @eval@.
--
-- For niche use cases, generating JSaddle-based 'JSaddle.eval' instead is
-- possible by disabling the @eval-via-jsffi@ Cabal flag.
--
-- For convenience, on non-Wasm GHCs, the semantics of having @eval-via-jsffi@
-- disabled are used.
module Language.Javascript.JSaddle.Wasm.TH where

import Language.Haskell.TH qualified as TH
import Language.Javascript.JSaddle qualified as JSaddle
#ifdef EVAL_VIA_JSFFI
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle.Wasm.Internal.TH qualified as Internal
#else
import Data.Functor (void)
#endif

-- | Generate an expression that, when called, evaluates the given chunk of JS
-- code. Additionally, a list of argument types can be specified.
--
-- For example,
--
-- > $(eval "console.log('hi')") :: JSM ()
--
-- will print \"hi\" to the console when executed.
--
-- Internally, this generates the following code:
--
--  * If the Cabal flag @eval-via-jsffi@ is enabled (the default): An
--    appropriate safe Wasm JSFFI import.
--
--  * If @eval-via-jsffi@ is disabled: Use JSaddle's 'JSaddle.eval'.
eval :: String -> TH.Q TH.Exp
#if EVAL_VIA_JSFFI
eval chunk = [|liftIO $(Internal.eval chunk []) :: JSaddle.JSM ()|]
#else
eval chunk = [|void $ JSaddle.eval chunk|]
#endif

-- | Like 'eval', but read the JS code to evaluate from a file.
evalFile :: FilePath -> TH.Q TH.Exp
evalFile path = do
  chunk <- TH.runIO $ readFile path
  eval chunk
