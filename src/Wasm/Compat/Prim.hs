{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

#if defined(wasm32_HOST_ARCH)
module Wasm.Compat.Prim (
  module GHC.Wasm.Prim
) where
import GHC.Wasm.Prim
#else
module Wasm.Compat.Prim (
  JSVal#(..),
  JSVal(..),
  freeJSVal,
  JSString (..),
  fromJSString,
  toJSString,
  JSException (..),
  WouldBlockException (..),
  PromisePendingException (..),
  mkJSCallback,
  runIO,
  runNonIO,
) where

import Control.Exception
import GHC.Exts
import GHC.Stable

newtype JSVal#
  = JSVal# (Any :: UnliftedType)

data JSVal
  = forall a . JSVal JSVal# (Weak# JSVal#) (StablePtr# a)

freeJSVal :: JSVal -> IO ()
freeJSVal _ = error "freeJSVal: only implmented on Wasm Backend"

newtype JSString = JSString JSVal

fromJSString :: JSString -> String
fromJSString _ = error "fromJSString: only implmented on Wasm Backend"

toJSString :: String -> JSString
toJSString _ = error "toJSString: only implmented on Wasm Backend"

newtype JSException = JSException JSVal

instance Show JSException where
  showsPrec _ _ = error "showsPrec @JSException: only implmented on Wasm Backend"

instance Exception JSException

data WouldBlockException
  = WouldBlockException
  deriving (Show)

instance Exception WouldBlockException

data PromisePendingException
  = PromisePendingException
  deriving (Show)

instance Exception PromisePendingException

mkJSCallback :: (StablePtr a -> IO JSVal) -> a -> IO JSVal
mkJSCallback _ _ = error "mkJSCallback: only implmented on Wasm Backend"

runIO :: (JSVal -> a -> IO ()) -> IO a -> IO JSVal
runIO _ _ = error "runIO: only implmented on Wasm Backend"

runNonIO :: (JSVal -> a -> IO ()) -> a -> IO JSVal
runNonIO _ _ = error "runNonIO: only implmented on Wasm Backend"
#endif
