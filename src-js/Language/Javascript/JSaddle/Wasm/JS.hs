-- | The JSaddle command interpreter. This lives in a sublibrary as it does not
-- depend on JSFFI as the rest of jsaddle-wasm, and hence does not induce this
-- property on downstream packages.
module Language.Javascript.JSaddle.Wasm.JS (jsaddleScript) where

import Data.ByteString.Lazy.Char8 qualified as BLC8
import Language.Javascript.JSaddle.Run.Files qualified as JSaddle.Files

-- | A chunk of JavaScript that defines a function @runJSaddle@, a function that
-- takes a message port (e.g. a web worker) as its single argument, and then
-- processes incoming JSaddle commands.
jsaddleScript :: BLC8.ByteString
jsaddleScript =
  BLC8.unlines
    [ JSaddle.Files.ghcjsHelpers,
      JSaddle.Files.initState,
      "function runJSaddle(worker) {",
      "  worker.addEventListener('message', e => {",
      "    const d = e.data;",
      "    if (d && typeof d === 'object' && d.tag === 'jsaddle') {",
      "      const batch = JSON.parse(d.msg);",
      JSaddle.Files.runBatch
        (\r -> "worker.postMessage({tag: 'jsaddle', msg: JSON.stringify(" <> r <> ")});")
        -- not clear how to support synchronous dispatch here
        Nothing,
      "    }",
      "  });",
      "}"
    ]
