{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Clickable.FFI where

import Wasm.Compat.Prim
import Control.Monad.IO.Class
import GHC.Ptr
import Data.Word
import Data.Text.Internal
import Data.Array.Byte
import GHC.Prim

consoleLog :: MonadIO m => Text -> m ()
consoleLog (Text (ByteArray arr) off len) = liftIO do
  let addr = byteArrayContents# arr
  js_consoleLog (Ptr addr `plusPtr` off) len

#if !defined(wasm32_HOST_ARCH)
js_evalMessageFFI :: JSVal -> Ptr Word8 -> IO (Ptr Word8)
js_evalMessageFFI = undefined

js_exportCallback :: (Ptr Word8 {- JavaScriptMessage -} -> IO ()) -> IO JSVal
js_exportCallback = undefined

js_consoleLog :: Ptr Word8 -> Int -> IO ()
js_consoleLog = undefined
#else
foreign import javascript unsafe
  "evalMessageFFI($1, __exports, $2)"
   js_evalMessageFFI :: JSVal -> Ptr Word8 {- HaskellMessage -} -> IO (Ptr Word8 {- JavaScriptMessage -})
foreign import javascript "wrapper"
   js_exportCallback :: (Ptr Word8 {- JavaScriptMessage -} -> IO ()) -> IO JSVal
foreign import javascript unsafe
  "console.log(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $1, $2)));"
  js_consoleLog :: Ptr Word8 -> Int -> IO ()
#endif
