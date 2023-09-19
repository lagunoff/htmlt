{-|
Implement the missing functionality, which is likely to be included in
the standard library at some point in the future.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Compat.Marshal where

import Data.Bool
import Data.Maybe
import Data.Coerce
import GHC.JS.Prim
import JavaScript.Compat.String
import Unsafe.Coerce

newtype Nullable v = Nullable {unNullable :: JSVal}

nullableToMaybe :: Coercible v JSVal => Nullable v -> Maybe v
nullableToMaybe (Nullable jsval)
  | isNull jsval = Nothing
  | otherwise    = Just (coerce jsval)

nullableFromMaybe :: Coercible v JSVal => Maybe v -> Nullable v
nullableFromMaybe = Nullable . maybe jsNull coerce

class FromJSVal v where fromJSVal :: JSVal -> IO (Maybe v)

instance FromJSVal JSString where
  fromJSVal jsval = do
    let
      isString = unsafeCoerce $
        js_bool (unsafeCoerce False) (unsafeCoerce True) (js_isString jsval)
    return $ if isString then Just (JSString jsval) else Nothing

instance FromJSVal Int where
  fromJSVal = pure . Just . fromJSInt

instance FromJSVal Bool where
  fromJSVal = pure . Just . unsafeCoerce .
    js_bool (unsafeCoerce False) (unsafeCoerce True)

instance FromJSVal JSVal where
  fromJSVal = pure . Just

instance FromJSVal v => FromJSVal (Maybe v) where
  fromJSVal j = maybe (pure (Just Nothing)) fromJSVal $
    nullableToMaybe (Nullable j)

instance FromJSVal v => FromJSVal [v] where
  fromJSVal s = fmap (Just . catMaybes) . mapM fromJSVal =<< fromJSArray s

class ToJSVal v where toJSVal :: v -> IO JSVal

instance ToJSVal JSString where
  toJSVal = pure . unJSString

instance ToJSVal Int where
  toJSVal = pure . toJSInt

instance ToJSVal Bool where
  toJSVal = pure . bool js_false js_true

instance ToJSVal JSVal where
  toJSVal = pure

instance ToJSVal v => ToJSVal (Maybe v) where
  toJSVal s = pure . unNullable . nullableFromMaybe =<< mapM toJSVal s

instance ToJSVal v => ToJSVal [v] where
  toJSVal s = toJSArray =<< mapM toJSVal s

#if !defined(javascript_HOST_ARCH)
js_true :: JSVal = undefined
js_false :: JSVal = undefined
js_isString :: JSVal -> JSVal = undefined
#else
foreign import javascript unsafe
  "(() => true)" js_true :: JSVal
foreign import javascript unsafe
  "(() => false)" js_false :: JSVal
foreign import javascript unsafe
  "((s) => typeof s === 'string')" js_isString :: JSVal -> JSVal
#endif
