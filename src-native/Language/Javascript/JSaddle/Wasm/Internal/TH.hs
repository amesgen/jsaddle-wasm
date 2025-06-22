module Language.Javascript.JSaddle.Wasm.Internal.TH where

import Language.Haskell.TH qualified as TH

eval :: String -> [TH.Q TH.Type] -> TH.Q TH.Exp
eval _jsChunk argTys = do
  f <- [e|fail "Language.Javascript.JSaddle.Wasm.TH.eval: only works on Wasm backend"|]
  [e|$(pure $ TH.LamE (TH.WildP <$ argTys) f) :: $(sig argTys)|]
  where
    sig = \case
      [] -> [t|IO ()|]
      t : ts -> [t|$t -> $(sig ts)|]
