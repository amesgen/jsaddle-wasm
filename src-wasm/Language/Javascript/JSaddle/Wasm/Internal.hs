module Language.Javascript.JSaddle.Wasm.Internal (run) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BI
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.ByteString.Unsafe qualified as BU
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString, JSVal)
import GHC.Wasm.Prim qualified
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files qualified as JSaddle.Files
import Language.Javascript.JSaddle.Types (Batch, JSM)

run :: JSM () -> IO ()
run entryPoint = do
  -- TODO rather use a bounded (even size 1) queue?
  batchQueue :: TQueue Batch <- newTQueueIO

  let sendBatch :: Batch -> IO ()
      sendBatch = atomically . writeTQueue batchQueue

  (processResult, _processSyncResult, start) <-
    runJavaScript sendBatch entryPoint

  processResultCallback <- mkPullCallback \s -> do
    bs <- jsStringToByteString s
    case A.eitherDecodeStrict bs of
      Left e -> fail $ "jsaddle: received invalid JSON: " <> show e
      Right r -> processResult r

  readBatchCallback <- mkPushCallback do
    batch <- atomically $ readTQueue batchQueue
    byteStringToJSString $ BLC8.toStrict $ A.encode batch

  let jsaddleRunner :: IO ()
      jsaddleRunner = do
        s <-
          byteStringToJSString . BLC8.toStrict . BLC8.unlines $
            [ JSaddle.Files.ghcjsHelpers,
              JSaddle.Files.initState,
              "(async () => {",
              "  while (true) {",
              "    const batch = JSON.parse(await readBatch());",
              JSaddle.Files.runBatch
                (\r -> "processResult(JSON.stringify(" <> r <> "));")
                -- TODO handle synchronous stuff (see processSyncResult)
                Nothing,
              "  }",
              "})();"
            ]
        evaluate =<< js_eval s processResultCallback readBatchCallback

  Async.concurrently_ start jsaddleRunner

-- Utilities:

foreign import javascript "wrapper" mkPullCallback :: (JSString -> IO ()) -> IO JSVal

foreign import javascript "wrapper" mkPushCallback :: IO JSString -> IO JSVal

foreign import javascript safe "new Function('processResult','readBatch',`(()=>{${$1}})()`)($2, $3)"
  js_eval :: JSString -> JSVal -> JSVal -> IO ()

-- Conversion JSString <-> ByteString

foreign import javascript unsafe "$1.length"
  js_stringLength :: JSString -> IO Int

foreign import javascript unsafe "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encodeInto :: JSString -> Ptr a -> Int -> IO Int

jsStringToByteString :: JSString -> IO ByteString
jsStringToByteString s = do
  len <- js_stringLength s
  -- see https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto#buffer_sizing
  -- (could also use another strategy described there)
  let lenMax = len * 3
  BI.createUptoN lenMax \buf -> js_encodeInto s buf lenMax

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_toJSString :: Ptr a -> Int -> IO JSString

byteStringToJSString :: ByteString -> IO JSString
byteStringToJSString bs =
  BU.unsafeUseAsCStringLen bs $ uncurry js_toJSString
