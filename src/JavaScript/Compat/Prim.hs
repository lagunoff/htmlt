{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}

#if defined(javascript_HOST_ARCH)
module JavaScript.Compat.Prim
  ( module GHC.JS.Prim
  ) where

import GHC.JS.Prim
#else

module JavaScript.Compat.Prim ( JSVal(..), JSVal#
                   , JSException(..)
                   , WouldBlockException(..)
                  , toIO
                  , resolve
                  , resolveIO
                  , mkJSException
                  , fromJSString
                  , toJSString
                  , toJSArray
                  , fromJSArray
                  , fromJSInt
                  , toJSInt
                  , isNull
                  , isUndefined
                  , jsNull
                  , getProp
                  , getProp'
                  , getProp#
                  , unsafeGetProp
                  , unsafeGetProp'
                  , unsafeGetProp#
                  , unpackJSString#
                  , unpackJSStringUtf8#
                  , unsafeUnpackJSString#
                  , unsafeUnpackJSStringUtf8#
                  , unpackJSStringUtf8##
                  , unsafeUnpackJSStringUtf8##
                  ) where

import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Prim
import qualified GHC.Exception as Ex
import qualified GHC.Exts as Exts
import qualified GHC.CString as GHC
import           GHC.IO

{-
  JSVal is a boxed type that can be used as FFI
  argument or result.
-}

data JSVal  = JSVal Addr#
type JSVal# = Addr#

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException JSVal String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

toIO :: Exts.Any -> IO Exts.Any
toIO = undefined

resolve :: JSVal# -> JSVal# -> Exts.Any -> IO ()
resolve = undefined

resolveIO :: JSVal# -> JSVal# -> IO Exts.Any -> IO ()
resolveIO = undefined

mkJSException :: JSVal -> IO JSException
mkJSException = undefined

{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

{- | returns an empty string if the JSVal does not contain
     a string
 -}
fromJSString :: JSVal -> String
fromJSString = undefined

toJSString :: String -> JSVal
toJSString = undefined

fromJSArray :: JSVal -> IO [JSVal]
fromJSArray = undefined

toJSArray :: [JSVal] -> IO JSVal
toJSArray = undefined

{- | returns zero if the JSVal does not contain a number
 -}
fromJSInt :: JSVal -> Int
fromJSInt = undefined

toJSInt :: Int -> JSVal
toJSInt = undefined

isNull :: JSVal -> Bool
isNull = undefined

isUndefined :: JSVal -> Bool
isUndefined = undefined

jsNull :: JSVal
jsNull = undefined

getProp :: JSVal -> String -> IO JSVal
getProp = undefined

-- | only safe on immutable object
unsafeGetProp :: JSVal -> String -> JSVal
unsafeGetProp = undefined

getProp' :: JSVal -> JSVal -> IO JSVal
getProp' = undefined

-- | only safe on immutable object
unsafeGetProp' :: JSVal -> JSVal -> JSVal
unsafeGetProp' = undefined


-- | only safe on immutable Addr#
getProp# :: JSVal -> Addr# -> IO JSVal
getProp# = undefined

-- | only safe on immutable Addr#
getPropUtf8# :: JSVal -> Addr# -> IO JSVal
getPropUtf8# = undefined

getPropUtf8## :: JSVal# -> Addr# -> State# s -> (# State# s, JSVal# #)
getPropUtf8## = undefined

-- | only safe on immutable Addr# and JSVal
unsafeGetProp# :: JSVal -> Addr# -> JSVal
unsafeGetProp# = undefined

-- | only safe on immutable Addr# and JSVal
unsafeGetPropUtf8# :: JSVal -> Addr# -> JSVal
unsafeGetPropUtf8# = undefined

unsafeGetPropUtf8## :: JSVal# -> Addr# -> JSVal#
unsafeGetPropUtf8## = undefined

unpackJSString# :: Addr# -> IO JSVal
unpackJSString# = undefined

unpackJSStringUtf8# :: Addr# -> IO JSVal
unpackJSStringUtf8# = undefined

unpackJSStringUtf8## :: Addr# -> State# s -> (# State# s, JSVal# #)
unpackJSStringUtf8## = undefined

-- | only safe on immutable Addr#
unsafeUnpackJSString# :: Addr# -> JSVal
unsafeUnpackJSString# = undefined

-- | only safe on immutable Addr#
unsafeUnpackJSStringUtf8# :: Addr# -> JSVal
unsafeUnpackJSStringUtf8# = undefined

unsafeUnpackJSStringUtf8## :: Addr# -> JSVal#
unsafeUnpackJSStringUtf8## = undefined

{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException
  deriving (Typeable)

instance Show WouldBlockException where
  show _ = "thread would block"

instance Ex.Exception WouldBlockException
#endif
