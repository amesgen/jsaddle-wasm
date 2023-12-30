module Language.Javascript.JSaddle.WASM (run) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Exception qualified as E
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BI
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.ByteString.Unsafe qualified as BU
import Foreign.Ptr (castPtr)
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

  processResultCallback <- mkPullCallback \s -> logException do
    bs <- jsStringToByteString s
    case A.eitherDecodeStrict bs of
      Left e -> fail $ "jsaddle: received invalid JSON: " <> show e
      Right r -> processResult r

  readBatchCallback <- mkPushCallback $ logException do
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
              "    const batch = JSON.parse(await ($3)());",
              JSaddle.Files.runBatch
                (\r -> "($2)(JSON.stringify(" <> r <> "));")
                -- TODO handle synchronous stuff (see processSyncResult)
                Nothing,
              "  }",
              "})();"
            ]
        js_eval s processResultCallback readBatchCallback

  Async.concurrently_ start jsaddleRunner

-- Utilities:

-- Logging

foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()

jsLog :: String -> IO ()
jsLog = js_log . GHC.Wasm.Prim.toJSString

logException :: IO a -> IO a
logException = E.handle \ex -> do
  jsLog $ "Haskell exception: " <> show (ex :: E.SomeException)
  E.throwIO ex

-- JS FFI

foreign import javascript "wrapper" mkPullCallback :: (JSString -> IO ()) -> IO JSVal

foreign import javascript "wrapper" mkPushCallback :: IO JSString -> IO JSVal

foreign import javascript safe "(new Function('$1','$2','$3',`(()=>{${$1}})()`))($1, $2, $3)"
  js_eval :: JSString -> JSVal -> JSVal -> IO ()

-- Conversion JSString <-> ByteString

jsStringToByteString :: JSString -> IO ByteString
jsStringToByteString s = do
  len <- GHC.Wasm.Prim.js_stringLength s
  -- see https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto#buffer_sizing
  -- (could also use another strategy described there)
  let lenMax = len * 3
  BI.createUptoN lenMax \buf ->
    GHC.Wasm.Prim.js_encodeInto s (castPtr buf) lenMax

byteStringToJSString :: ByteString -> IO JSString
byteStringToJSString bs =
  BU.unsafeUseAsCStringLen bs $ uncurry GHC.Wasm.Prim.js_toJSString
