module Language.Javascript.JSaddle.Wasm.Internal
  ( run,
    runWorker,
    JSVal,
  )
where

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad ((<=<))
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

-- Note: It is also possible to implement this succinctly on top of 'runWorker'
-- and 'jsaddleScript' (using MessageChannel), but then e.g.
-- @stopPropagation@/@preventDefault@ definitely don't work, whereas they work
-- (at least in simple cases) with the implementation below.
run :: JSM () -> IO ()
run entryPoint = do
  -- TODO rather use a bounded (even size 1) queue?
  outgoingMsgQueue :: TQueue JSString <- newTQueueIO

  let sendOutgoingMessage =
        atomically . writeTQueue outgoingMsgQueue

  readBatchCallback <-
    mkPushCallback $ atomically $ readTQueue outgoingMsgQueue

  let jsaddleRunner :: JSVal -> IO ()
      jsaddleRunner processResultCallback = do
        s <-
          byteStringToJSString . BLC8.toStrict . BLC8.unlines $
            [ JSaddle.Files.ghcjsHelpers,
              JSaddle.Files.initState,
              "(async () => {",
              "  while (true) {",
              "    const batch = JSON.parse(await readBatch());",
              JSaddle.Files.runBatch
                (\r -> "processResult(JSON.stringify(" <> r <> "));")
                -- TODO Think more about how synchronous dispatching works here
                -- (see processSyncResult). For some reason, it already /seems/
                -- to work fine, at least in simple cases.
                Nothing,
              "  }",
              "})();"
            ]
        evaluate =<< js_eval s processResultCallback readBatchCallback

  runHelper entryPoint sendOutgoingMessage jsaddleRunner

runWorker :: JSM () -> JSVal -> IO ()
runWorker entryPoint worker =
  runHelper
    entryPoint
    (evaluate <=< js_postMessage worker)
    (evaluate <=< js_onMessage worker)

runHelper ::
  JSM () ->
  -- | How to send an outgoing message.
  (JSString -> IO ()) ->
  -- | Start receiving incoming messages. For every message, invoke the given
  -- 'JSVal' callback.
  (JSVal -> IO ()) ->
  IO ()
runHelper entryPoint sendOutgoingMessage onIncomingMessage = do
  (processResult, _processSyncResult, start) <-
    runJavaScript sendBatch entryPoint

  processResultCallback <- mkPullCallback \s -> do
    bs <- jsStringToByteString s
    case A.eitherDecodeStrict bs of
      Left e -> fail $ "jsaddle: received invalid JSON: " <> show e
      Right r -> processResult r

  onIncomingMessage processResultCallback

  start
  where
    sendBatch :: Batch -> IO ()
    sendBatch batch = do
      encodedBatch <- byteStringToJSString $ BLC8.toStrict $ A.encode batch
      sendOutgoingMessage encodedBatch

-- Utilities:

foreign import javascript "wrapper" mkPullCallback :: (JSString -> IO ()) -> IO JSVal

foreign import javascript "wrapper" mkPushCallback :: IO JSString -> IO JSVal

foreign import javascript safe "new Function('processResult','readBatch',`(()=>{${$1}})()`)($2, $3)"
  js_eval :: JSString -> JSVal -> JSVal -> IO ()

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
