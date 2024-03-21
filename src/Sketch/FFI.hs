{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Sketch.FFI where

import Control.Monad.IO.Class
import Wasm.Compat.Prim
import GHC.Ptr
import Data.Word
import GHC.Prim
import Data.Text.Internal
import Data.Array.Byte


insertText :: JSVal -> Text -> IO JSVal
insertText root (Text (ByteArray arr) off len) = do
  let addr = byteArrayContents# arr
  js_insertText root (Ptr addr `plusPtr` off) len

insertElement :: JSVal -> Text -> IO JSVal
insertElement root (Text (ByteArray arr) off len) = do
  let addr = byteArrayContents# arr
  js_insertElement root (Ptr addr `plusPtr` off) len

updateTextContent :: JSVal -> Text -> IO ()
updateTextContent root (Text (ByteArray arr) off len) = do
  let addr = byteArrayContents# arr
  js_updateTextContent root (Ptr addr `plusPtr` off) len

setProperty :: JSVal -> Text -> Text -> IO ()
setProperty root (Text (ByteArray arr0) off0 len0) (Text (ByteArray arr1) off1 len1) = do
  let addr0 = byteArrayContents# arr0
  let addr1 = byteArrayContents# arr1
  js_setProperty root (Ptr addr0 `plusPtr` off0) len0 (Ptr addr1 `plusPtr` off1) len1

addEventListener :: JSVal -> Text -> JSVal -> IO ()
addEventListener root (Text (ByteArray arr) off len) lisnr = do
  let addr = byteArrayContents# arr
  js_addEventListener root (Ptr addr `plusPtr` off) len lisnr

consoleLog :: MonadIO m => Text -> m ()
consoleLog (Text (ByteArray arr) off len) = liftIO do
  let addr = byteArrayContents# arr
  js_consoleLog (Ptr addr `plusPtr` off) len

#if !defined(wasm32_HOST_ARCH)
js_insertText :: JSVal -> Ptr Word8 -> Int -> IO JSVal
js_insertText = undefined

js_insertElement :: JSVal -> Ptr Word8 -> Int -> IO JSVal
js_insertElement = undefined

js_updateTextContent :: JSVal -> Ptr Word8 -> Int -> IO ()
js_updateTextContent = undefined

js_setProperty :: JSVal -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
js_setProperty = undefined

js_addEventListener :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
js_addEventListener = undefined

js_dynExport :: (JSVal -> IO ()) -> IO JSVal
js_dynExport = undefined

documentBody :: IO JSVal
documentBody = undefined

js_consoleLog :: Ptr Word8 -> Int -> IO ()
js_consoleLog = undefined

#else
foreign import javascript unsafe
  "var c = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var n = document.createTextNode(c);\
   $1.appendChild(n);\
   return n;"
  js_insertText :: JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "var t = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var n = document.createElement(t);\
   $1.appendChild(n);\
   return n;"
  js_insertElement :: JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "$1.nodeValue = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));"
  js_updateTextContent :: JSVal -> Ptr Word8 -> Int -> IO ()
foreign import javascript unsafe
  "var k = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var v = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $4, $5));\
   $1[k] = v;"
  js_setProperty :: JSVal -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
foreign import javascript unsafe "document.body" documentBody :: IO JSVal
foreign import javascript unsafe
  "var e = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   $1.addEventListener(e, $4);"
  js_addEventListener :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
foreign import javascript "wrapper" js_dynExport :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript unsafe
  "console.log(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $1, $2)));"
  js_consoleLog :: Ptr Word8 -> Int -> IO ()
#endif
