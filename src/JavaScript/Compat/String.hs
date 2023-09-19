{-|
Implement the missing functionality, which is likely to be included in
the standard library at some point in the future.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Compat.String where

import GHC.Exts as Exts
import JavaScript.Compat.Prim
import System.IO.Unsafe
import Unsafe.Coerce

newtype JSString = JSString {unJSString :: JSVal}

instance Show JSString where
  show = show . unpack
  {-# INLINE show #-}

instance Eq JSString where
  (==) (JSString a) (JSString b) = unsafeCoerce $
    js_stringEq a b (unsafeCoerce True) (unsafeCoerce False)
  {-# INLINE (==) #-}

instance Ord JSString where
  -- TODO: Could be made way faster once implemented in JavaScript
  compare a b = unpack a `compare` unpack b

instance IsString JSString where
  fromString = JSString . toJSString
  {-# INLINE fromString #-}

instance Semigroup JSString where
  (<>) = append
  {-# INLINE (<>) #-}

instance Monoid JSString where
  mempty = empty
  {-# INLINE mempty #-}

empty :: JSString
empty = ""
{-# NOINLINE empty #-}

pack :: String -> JSString
pack = JSString . toJSString
{-# INLINE pack #-}

unpack :: JSString -> String
unpack = fromJSString . unJSString
{-# INLINE unpack #-}

strip :: JSString -> JSString
strip = JSString . js_strip . unJSString
{-# INLINE strip #-}

append :: JSString -> JSString -> JSString
append (JSString a) (JSString b) = JSString (js_append a b)
{-# INLINE append #-}

stripPrefix :: JSString -> JSString -> Maybe JSString
stripPrefix (JSString str) (JSString prefix) =
  let
    jsVal = js_stripPrefix str prefix
  in
    if isUndefined jsVal then Nothing else Just (JSString jsVal)
{-# INLINE stripPrefix #-}

breakOn :: JSString -> JSString -> (JSString, JSString)
breakOn (JSString sep) (JSString str) =
  let
    jsTuple = js_breakOn sep str
  in
    ( JSString (js_unsafeIndex 0 jsTuple)
    , JSString (js_unsafeIndex 1 jsTuple)
    )
{-# INLINE breakOn #-}

splitOn :: JSString -> JSString -> [JSString]
splitOn (JSString sep) (JSString str) = fmap JSString $ unsafePerformIO $
  fromJSArray (js_splitOn sep str)
{-# NOINLINE splitOn #-}

intercalate :: JSString -> [JSString] -> JSString
intercalate (JSString sep) list =
  let
    jsList = unsafePerformIO $ toJSArray (fmap unJSString list)
  in
    JSString $ js_intercalate sep jsList
{-# NOINLINE intercalate #-}

drop :: Int -> JSString -> JSString
drop n (JSString str) = JSString $ js_drop n str
{-# INLINE drop #-}

take :: Int -> JSString -> JSString
take n (JSString str) = JSString $ js_take n str
{-# INLINE take #-}

encodeURIComponent :: JSString -> JSString
encodeURIComponent =
  JSString . js_encodeURIComponent . unJSString
{-# INLINE encodeURIComponent #-}

decodeURIComponent :: JSString -> JSString
decodeURIComponent =
  JSString . js_decodeURIComponent . unJSString
{-# INLINE decodeURIComponent #-}

toLower :: JSString -> JSString
toLower = JSString . js_toLower . unJSString
{-# INLINE toLower #-}

toUpper :: JSString -> JSString
toUpper = JSString . js_toUpper . unJSString
{-# INLINE toUpper #-}

isInfixOf :: JSString -> JSString -> Bool
isInfixOf (JSString substr) (JSString str) =
  unsafeCoerce $ js_bool (unsafeCoerce False) (unsafeCoerce True) (js_isInfixOf substr str)
{-# INLINE isInfixOf #-}

null :: JSString -> Bool
null = (==empty)
{-# INLINE null #-}

#if !defined(javascript_HOST_ARCH)
js_stringEq :: JSVal -> JSVal -> Exts.Any -> Exts.Any -> Exts.Any = undefined
js_strip :: JSVal -> JSVal = undefined
js_append :: JSVal -> JSVal -> JSVal = undefined
js_stripPrefix :: JSVal -> JSVal -> JSVal = undefined
js_splitOn :: JSVal-> JSVal -> JSVal = undefined
js_breakOn :: JSVal-> JSVal -> JSVal = undefined
js_unsafeIndex :: Int -> JSVal -> JSVal = undefined
js_intercalate :: JSVal -> JSVal -> JSVal = undefined
js_drop :: Int -> JSVal -> JSVal = undefined
js_take :: Int -> JSVal -> JSVal = undefined
js_encodeURIComponent :: JSVal -> JSVal = undefined
js_decodeURIComponent :: JSVal -> JSVal = undefined
js_toLower :: JSVal -> JSVal = undefined
js_toUpper :: JSVal -> JSVal = undefined
js_isInfixOf :: JSVal -> JSVal -> JSVal = undefined
js_bool :: Exts.Any -> Exts.Any -> JSVal -> Exts.Any = undefined
#else
foreign import javascript unsafe
  "((lhs, rhs, ifeq, ifneq) => lhs == rhs ? ifeq : ifneq)"
  js_stringEq :: JSVal -> JSVal -> Exts.Any -> Exts.Any -> Exts.Any
foreign import javascript unsafe
  "((str) => str.replace(/^\\s+|\\s+$/g, ''))"
  js_strip :: JSVal -> JSVal
foreign import javascript unsafe
  "((a, b) => a + b)"
  js_append :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "((prefix, str) => {\
    if (str.startsWith(prefix)) {\
      return str.slice(prefix.length);\
    }\
    return undefined;\
  })"
  js_stripPrefix :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "((sep, str) => str.split(sep))"
  js_splitOn :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "((sep, str) => {\
    var index = str.indexOf(sep);\
    if (index !== -1) {\
      return [str.slice(0, index), str.slice(index)];\
    }\
    return [str, ''];\
   })"
  js_breakOn :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "((i, arr) => arr[i])"
  js_unsafeIndex :: Int -> JSVal -> JSVal
foreign import javascript unsafe
  "((sep, list) => list.join(sep))"
  js_intercalate :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "((n, str) => str.slice(n))"
  js_drop :: Int -> JSVal -> JSVal
foreign import javascript unsafe
  "((n, str) => str.slice(0, n))"
  js_take :: Int -> JSVal -> JSVal
foreign import javascript unsafe
  "encodeURIComponent"
  js_encodeURIComponent :: JSVal -> JSVal
foreign import javascript unsafe
  "decodeURIComponent"
  js_decodeURIComponent :: JSVal -> JSVal
foreign import javascript unsafe
  "((s) => s.toLowerCase())"
  js_toLower :: JSVal -> JSVal
foreign import javascript unsafe
  "((s) => s.toUpperCase())"
  js_toUpper :: JSVal -> JSVal
foreign import javascript unsafe
  "((substr, str) => str.includes(substr))"
  js_isInfixOf :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "((iffalse, iftrue, jsbool) => jsbool ? iftrue : iffalse)"
  js_bool :: Exts.Any -> Exts.Any -> JSVal -> Exts.Any
#endif
