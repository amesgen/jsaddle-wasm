module Language.Javascript.JSaddle.Wasm.Internal.TH
  ( eval,
    patchedGhcjsHelpers,
  )
where

import Control.Applicative (asum, many)
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Language.Javascript.JSaddle.Run.Files qualified as JSaddle.Files
import Regex.List qualified as Re

eval :: String -> [TH.Q TH.Type] -> TH.Q TH.Exp
eval jsChunk argTys = do
  ffiImportName <- TH.newName . show =<< TH.newName "wasm_ffi_import_eval"
  sig <- mkSig argTys
  let ffiImport =
        TH.ForeignD $
          TH.ImportF
            TH.JavaScript
            TH.Safe
            jsChunk
            ffiImportName
            sig
  TH.addTopDecls [ffiImport]

  argNames <- traverse (\_ -> TH.newName "x") argTys
  let argPats = TH.varP <$> argNames
      argExps = TH.varE <$> argNames
  -- Safe FFI imports return a thunk that needs to be evaluated to make sure
  -- that the FFI call actually completed ('unsafeInterleaveIO'-like). To avoid
  -- surprises, use this unconditionally.
  TH.lamE argPats [|evaluate =<< $(TH.appsE $ TH.varE ffiImportName : argExps)|]
  where
    mkSig = \case
      [] -> [t|IO ()|]
      t : ts -> [t|$t -> $(mkSig ts)|]

-- | The JSaddle GHCJS helpers need to be available in the global scope.
-- Usually, this is done by evaluating them in a global scope; however, we want
-- to avoid JS eval (due to CSP, see
-- https://github.com/tweag/ghc-wasm-miso-examples/issues/33), so we instead use
-- a hack, namely transforming
--
-- > function foo(a) {
-- >   return a + 1;
-- > }
--
-- into
--
-- > globalThis["foo"] = function(a) {
-- >   return a + 1;
-- > }
--
-- Of course, this only works because of the very particular structure of
-- 'ghcjsHelpers'; but it changes very rarely (didn't change non-trivially in
-- the last 10 years), so this seems acceptable.
patchedGhcjsHelpers :: String
patchedGhcjsHelpers =
  Re.replaceAll re $ BLC8.unpack JSaddle.Files.ghcjsHelpers
  where
    re :: Re.RE Char String
    re =
      asum
        [ f <$> (Re.list "function " *> many (Re.satisfy (/= '('))),
          "\n};" <$ Re.list "\n}"
        ]
      where
        f name = "globalThis[" <> show name <> "] = function"
