{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Clickable.FFI where

import Wasm.Compat.Prim
import GHC.Ptr
import Data.Word

#if !defined(wasm32_HOST_ARCH)
js_evalMessageFFI :: JSVal -> Ptr Word8 -> IO (Ptr Word8)
js_evalMessageFFI = undefined

js_exportCallback :: (Ptr Word8 {- JavaScriptMessage -} -> IO ()) -> IO JSVal
js_exportCallback = undefined

#else
foreign import javascript unsafe
  "evalMessageFFI($1, __exports, $2)"
   js_evalMessageFFI :: JSVal -> Ptr Word8 {- HaskellMessage -} -> IO (Ptr Word8 {- JavaScriptMessage -})
foreign import javascript "wrapper"
   js_exportCallback :: (Ptr Word8 {- JavaScriptMessage -} -> IO ()) -> IO JSVal
#endif
