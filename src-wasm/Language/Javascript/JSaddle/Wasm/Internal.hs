{-# LANGUAGE TemplateHaskell #-}

module Language.Javascript.JSaddle.Wasm.Internal
  ( run,
    runWorker,
    JSVal,
  )
where

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad ((<=<), (>=>))
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
import Language.Javascript.JSaddle.Types (Batch, JSM, Results)
import Language.Javascript.JSaddle.Wasm.Internal.TH qualified as JSaddle.Wasm.TH

-- Note: It is also possible to implement this succinctly on top of 'runWorker'
-- and 'jsaddleScript' (using MessageChannel), but then e.g.
-- @stopPropagation@/@preventDefault@ don't work, whereas the implementation
-- below has special support via @processResultSync@.
run :: JSM () -> IO ()
run entryPoint = do
  -- TODO rather use a bounded (even size 1) queue?
  outgoingMsgQueue :: TQueue JSString <- newTQueueIO

  let sendOutgoingMessage =
        atomically . writeTQueue outgoingMsgQueue

  readBatchCallback <-
    mkPushCallback $ atomically $ readTQueue outgoingMsgQueue

  let jsaddleRunner :: JSVal -> JSVal -> IO ()
      jsaddleRunner processResultCallback processResultSyncCallback = do
        let eval :: JSVal -> JSVal -> JSVal -> IO ()
            eval =
              $( do
                   let s =
                         BLC8.unlines $
                           [ JSaddle.Files.initState,
                             "var syncDepth = 0;",
                             "(async () => {",
                             "  while (true) {",
                             "    const batch = JSON.parse(await $3());",
                             JSaddle.Files.runBatch
                               (\r -> "$1(JSON.stringify(" <> r <> "));")
                               (Just \r -> "JSON.parse($2(JSON.stringify(" <> r <> ")))"),
                             "  }",
                             "})();"
                           ]
                   JSaddle.Wasm.TH.eval (BLC8.unpack s) (replicate 3 [t|JSVal|])
               )
        eval
          processResultCallback
          processResultSyncCallback
          readBatchCallback

  $(JSaddle.Wasm.TH.eval JSaddle.Wasm.TH.patchedGhcjsHelpers [])

  runHelper entryPoint sendOutgoingMessage jsaddleRunner

runWorker :: JSM () -> JSVal -> IO ()
runWorker entryPoint worker =
  runHelper
    entryPoint
    (evaluate <=< js_postMessage worker)
    (\processResult _processResultSync -> evaluate =<< js_onMessage worker processResult)

runHelper ::
  JSM () ->
  -- | How to send an outgoing message.
  (JSString -> IO ()) ->
  -- | Start receiving incoming messages. For every message, invoke one of the
  -- two given 'JSVal' callbacks (for sync/async processing, respectively).
  (JSVal -> JSVal -> IO ()) ->
  IO ()
runHelper entryPoint sendOutgoingMessage onIncomingMessage = do
  (processResult, processSyncResult, start) <-
    runJavaScript sendBatch entryPoint

  let receiveBatch :: JSString -> IO ()
      receiveBatch = decodeResults >=> processResult

      processBatchSync :: JSString -> IO JSString
      processBatchSync = decodeResults >=> processSyncResult >=> encodeBatch

  processResultCallback <- mkPullCallback receiveBatch
  processResultSyncCallback <- mkSyncCallback processBatchSync

  onIncomingMessage processResultCallback processResultSyncCallback

  start
  where
    sendBatch :: Batch -> IO ()
    sendBatch = encodeBatch >=> sendOutgoingMessage

    encodeBatch :: Batch -> IO JSString
    encodeBatch = byteStringToJSString . BLC8.toStrict . A.encode

    decodeResults :: JSString -> IO Results
    decodeResults s = do
      bs <- jsStringToByteString s
      case A.eitherDecodeStrict bs of
        Left e -> fail $ "jsaddle: received invalid JSON: " <> show e
        Right r -> pure r

-- Utilities:

foreign import javascript "wrapper" mkPullCallback :: (JSString -> IO ()) -> IO JSVal

foreign import javascript "wrapper sync" mkSyncCallback :: (JSString -> IO JSString) -> IO JSVal

foreign import javascript "wrapper" mkPushCallback :: IO JSString -> IO JSVal

-- Worker

foreign import javascript safe "$1.postMessage({tag: 'jsaddle', msg: $2})"
  js_postMessage :: JSVal -> JSString -> IO ()

foreign import javascript safe "$1.addEventListener('message', e => {\
    \const d = e.data;\
    \if (d && typeof d === 'object' && d.tag === 'jsaddle') $2(d.msg);\
  \})"
  js_onMessage :: JSVal -> JSVal -> IO ()

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
