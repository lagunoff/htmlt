{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Clickable.FFI where

import Control.Monad.IO.Class
import Wasm.Compat.Prim
import GHC.Ptr
import Data.Word
import GHC.Prim
import Data.Text.Internal
import Data.Array.Byte


newtype DomBuilder = DomBuilder {unDomBuilder :: JSVal}

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

setBoolProperty :: JSVal -> Text -> Bool -> IO ()
setBoolProperty root (Text (ByteArray arr0) off0 len0) val = do
  let addr0 = byteArrayContents# arr0
  js_setBoolProperty root (Ptr addr0 `plusPtr` off0) len0 (if val then 1 else 0)

setAttribute :: JSVal -> Text -> Text -> IO ()
setAttribute root (Text (ByteArray arr0) off0 len0) (Text (ByteArray arr1) off1 len1) = do
  let addr0 = byteArrayContents# arr0
  let addr1 = byteArrayContents# arr1
  js_setAttribute root (Ptr addr0 `plusPtr` off0) len0 (Ptr addr1 `plusPtr` off1) len1

addEventListener :: JSVal -> Text -> JSVal -> IO ()
addEventListener root (Text (ByteArray arr) off len) lisnr = do
  let addr = byteArrayContents# arr
  js_addEventListener root (Ptr addr `plusPtr` off) len lisnr

removeEventListener :: JSVal -> Text -> JSVal -> IO ()
removeEventListener root (Text (ByteArray arr) off len) lisnr = do
  let addr = byteArrayContents# arr
  js_removeEventListener root (Ptr addr `plusPtr` off) len lisnr

consoleLog :: MonadIO m => Text -> m ()
consoleLog (Text (ByteArray arr) off len) = liftIO do
  let addr = byteArrayContents# arr
  js_consoleLog (Ptr addr `plusPtr` off) len

aquireResource :: JSVal -> Text -> JSVal -> IO JSVal
aquireResource root (Text (ByteArray arr) off len) lisnr = do
  let addr = byteArrayContents# arr
  js_aquireResource root (Ptr addr `plusPtr` off) len lisnr

apply0 :: JSVal -> IO ()
apply0 = js_apply0

#if !defined(wasm32_HOST_ARCH)
js_insertText :: JSVal -> Ptr Word8 -> Int -> IO JSVal
js_insertText = undefined

js_insertElement :: JSVal -> Ptr Word8 -> Int -> IO JSVal
js_insertElement = undefined

js_updateTextContent :: JSVal -> Ptr Word8 -> Int -> IO ()
js_updateTextContent = undefined

js_setProperty :: JSVal -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
js_setProperty = undefined

js_setBoolProperty :: JSVal -> Ptr Word8 -> Int -> Int -> IO ()
js_setBoolProperty = undefined

js_setAttribute :: JSVal -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
js_setAttribute = undefined

js_addEventListener :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
js_addEventListener = undefined

js_removeEventListener :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
js_removeEventListener = undefined

js_dynExport :: (JSVal -> IO ()) -> IO JSVal
js_dynExport = undefined

documentBody :: IO JSVal
documentBody = undefined

js_consoleLog :: Ptr Word8 -> Int -> IO ()
js_consoleLog = undefined

js_insertBrackets :: JSVal -> IO JSVal
js_insertBrackets = undefined

js_clearBrackets :: JSVal -> IO ()
js_clearBrackets = undefined

js_removeBrackets :: JSVal -> IO ()
js_removeBrackets = undefined

js_aquireResource :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO JSVal
js_aquireResource = undefined

js_apply0 :: JSVal -> IO ()
js_apply0 = undefined

js_evalMessageFFI :: Ptr Word8 -> IO (Ptr Word8)
js_evalMessageFFI = undefined

#else
foreign import javascript unsafe
  "var c = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var n = document.createTextNode(c);\
   if ($1 instanceof Comment) {\
     $1.parentNode.insertBefore(n, $1);\
   } else {\
     $1.appendChild(n);\
   }\
   return n;"
  js_insertText :: JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "var t = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var n = document.createElement(t);\
   if ($1 instanceof Comment) {\
     $1.parentNode.insertBefore(n, $1);\
   } else {\
     $1.appendChild(n);\
   }\
   return n;"
  js_insertElement :: JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "$1.nodeValue = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));"
  js_updateTextContent :: JSVal -> Ptr Word8 -> Int -> IO ()
foreign import javascript unsafe
  "var k = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var v = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $4, $5));\
   if ($1 instanceof Comment) {\
     $1.parentNode[k] = v;\
   } else {\
     $1[k] = v;\
   }"
  js_setProperty :: JSVal -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
foreign import javascript unsafe
  "var k = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   if ($1 instanceof Comment) {\
     $1.parentNode[k] = $4 == 0 ? false : true;\
   } else {\
     $1[k] = $4 == 0 ? false : true;\
   }"
  js_setBoolProperty :: JSVal -> Ptr Word8 -> Int -> Int -> IO ()
foreign import javascript unsafe
  "var k = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   var v = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $4, $5));\
   if ($1 instanceof Comment) {\
     $1.parentNode.setAttribute(k, v);\
   } else {\
     $1.setAttribute(k, v);\
   }"
  js_setAttribute :: JSVal -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO ()
foreign import javascript unsafe "document.body" documentBody :: IO JSVal
foreign import javascript unsafe
  "var e = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   if ($1 instanceof Comment) {\
     $1.parentNode.addEventListener(e, $4);\
   } else {\
     $1.addEventListener(e, $4);\
   }"
  js_addEventListener :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
foreign import javascript unsafe
  "var e = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   if ($1 instanceof Comment) {\
     $1.parentNode.addEventListener(e, $4);\
   } else {\
     $1.removeEventListener(e, $4);\
   }"
  js_removeEventListener :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
foreign import javascript "wrapper" js_dynExport :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript unsafe
  "console.log(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $1, $2)));"
  js_consoleLog :: Ptr Word8 -> Int -> IO ()
foreign import javascript unsafe
  "var c1 = document.createComment('ContentBoundary {{');\
   var c2 = document.createComment('}}');\
   if ($1 instanceof Comment) {\
     $1.parentNode.insertBefore(c1, $1);\
     $1.parentNode.insertBefore(c2, $1);\
   } else {\
     $1.appendChild(c1);\
     $1.appendChild(c2);\
   }\
   return c2;"
  js_insertBrackets :: JSVal -> IO JSVal
foreign import javascript unsafe
  "function isOpenBracket(node) {return node instanceof Comment && node.textContent == 'ContentBoundary {{'}\
   function isCloseBracket(node) {return node instanceof Comment && node.textContent == '}}'}\
   var iter = $1;\
   var nestedCounter = 0;\
   for (;;){\
     if (!iter.previousSibling ||\
       (nestedCounter == 0 && isOpenBracket(iter.previousSibling))\
       ) break;\
     if (isCloseBracket(iter.previousSibling)) nestedCounter++;\
     else if (isOpenBracket(iter.previousSibling)) nestedCounter--;\
     iter.previousSibling.parentNode.removeChild(iter.previousSibling);\
   }"
  js_clearBrackets :: JSVal -> IO ()
foreign import javascript unsafe
  "function isOpenBracket(node) {return node instanceof Comment && node.textContent == 'ContentBoundary {{'}\
   function isCloseBracket(node) {return node instanceof Comment && node.textContent == '}}'}\
   var iter = $1;\
   var nestedCounter = 0;\
   for (;;){\
     if (!iter.previousSibling ||\
       (nestedCounter == 0 && isOpenBracket(iter.previousSibling))\
       ) break;\
     if (isCloseBracket(iter.previousSibling)) nestedCounter++;\
     else if (isOpenBracket(iter.previousSibling)) nestedCounter--;\
     iter.previousSibling.parentNode.removeChild(iter.previousSibling);\
   }\
   $1.parentNode($1);\
   if ($1 != iter) iter.parentNode.removeChild(iter);\
   "
  js_removeBrackets :: JSVal -> IO ()
foreign import javascript unsafe
  "var j = new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3));\
   if ($1 instanceof Comment) {\
     return eval(j)($1.parentNode, $4);\
   } else {\
     return eval(j)($1, $4);\
   }"
  js_aquireResource :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO JSVal
foreign import javascript unsafe
  "$1()" js_apply0 :: JSVal -> IO ()
foreign import javascript unsafe
  "evalMessageFFI(__exports, $1)"
   js_evalMessageFFI :: Ptr Word8 {- HaskellMessage -} -> IO (Ptr Word8 {- JavaScriptMessage -})
#endif
