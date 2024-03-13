{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Wasm.Compat.Marshal where

import Control.Monad
import Data.Maybe
import Data.String
import Data.Coerce
import Wasm.Compat.Prim

newtype Nullable v = Nullable {unNullable :: JSVal}

nullableToMaybe :: Coercible v JSVal => Nullable v -> Maybe v
nullableToMaybe (Nullable j)
  | js_isNull j > 0 = Nothing
  | otherwise       = Just (coerce j)

maybeToNullable :: Coercible v JSVal => Maybe v -> Nullable v
maybeToNullable = Nullable . maybe js_null coerce

newtype TypeResult = TypeResult {unTypeResult :: Int}

pattern TypeNull, TypeBool, TypeNumber, TypeString, TypeArray, TypeObject :: TypeResult
pattern TypeNull = TypeResult 0
pattern TypeBool = TypeResult 1
pattern TypeNumber = TypeResult 2
pattern TypeString = TypeResult 3
pattern TypeArray = TypeResult 4
pattern TypeObject = TypeResult 5

class FromJSVal v where fromJSVal :: JSVal -> IO (Maybe v)

class FromJSValPure v where fromJSValPure :: JSVal -> Maybe v

instance {-# OVERLAPS #-} FromJSValPure v => FromJSVal v where
  fromJSVal = pure . fromJSValPure

instance FromJSValPure Int where
  fromJSValPure j = case js_typeOf j of
    TypeNumber -> Just (js_unsafeInt j)
    _ -> Nothing

instance FromJSValPure v => FromJSValPure (Maybe v) where
  fromJSValPure j
    = maybe (Just Nothing) fromJSValPure
    $ nullableToMaybe (Nullable j)

instance FromJSValPure JSVal where fromJSValPure = Just

instance FromJSValPure JSString where
  fromJSValPure j = case js_typeOf j of
    TypeString -> Just (JSString j)
    _ -> Nothing

instance FromJSValPure Bool where
  fromJSValPure j = case js_typeOf j of
    TypeBool -> Just (js_unsafeBool j)
    _ -> Nothing

instance FromJSVal v => FromJSVal [v] where
  fromJSVal s = case js_typeOf s of
    TypeArray -> do
      len <- js_arrayLength s
      xs <- forM [0..(len - 1)] $ fromJSVal <=< js_arrayIndex s
      return $ Just $ catMaybes xs
    _ -> return Nothing

class ToJSVal v where toJSVal :: v -> IO JSVal

class ToJSValPure v where toJSValPure :: v -> JSVal

instance {-# OVERLAPS #-} ToJSValPure v => ToJSVal v where
  toJSVal = pure . toJSValPure

instance ToJSValPure Int where toJSValPure = js_intJSVal

instance ToJSValPure Bool where toJSValPure = js_boolJSVal

instance ToJSValPure JSVal where toJSValPure j = j

instance ToJSValPure JSString where toJSValPure (JSString j) = j

instance ToJSValPure v => ToJSValPure (Maybe v) where
  toJSValPure = unNullable . maybeToNullable . fmap toJSValPure

instance ToJSVal v => ToJSVal [v] where
  toJSVal s = do
    arr <- js_newEmptyArray
    forM_ s $ toJSVal >=> js_arrayPush arr
    return arr

instance IsString JSString where fromString = toJSString

instance Semigroup JSString where (<>) = js_concatStr

instance Monoid JSString where mempty = js_emptyStr

instance Eq JSString where (==) a b = fromJSString a == fromJSString b

instance Show JSString where show = show . fromJSString

instance Ord JSString where compare a b = fromJSString a `compare` fromJSString b

#if !defined(wasm32_HOST_ARCH)

js_true :: JSVal = undefined
js_false :: JSVal = undefined
js_null :: JSVal = undefined
js_emptyStr :: JSString = undefined
js_isString :: JSVal -> JSVal = undefined
js_isNull :: JSVal -> Int = undefined
js_typeOf :: JSVal -> TypeResult = undefined
js_unsafeInt :: JSVal -> Int = undefined
js_unsafeBool :: JSVal -> Bool = undefined
js_intJSVal :: Int -> JSVal = undefined
js_boolJSVal :: Bool -> JSVal = undefined
js_getProp :: JSVal -> JSString -> IO JSVal = undefined
js_concatStr :: JSString -> JSString -> JSString = undefined
js_newEmptyArray :: IO JSVal = undefined
js_arrayPush :: JSVal -> JSVal -> IO () = undefined
js_arrayLength :: JSVal -> IO Int = undefined
js_arrayIndex :: JSVal -> Int -> IO JSVal = undefined

#else

foreign import javascript unsafe
  "true" js_true :: JSVal
foreign import javascript unsafe
  "false" js_false :: JSVal
foreign import javascript unsafe
  "null" js_null :: JSVal
foreign import javascript unsafe
  "''" js_emptyStr :: JSString
foreign import javascript unsafe
  "typeof $1 === 'string'" js_isString :: JSVal -> JSVal
foreign import javascript unsafe
  "($1 === null || $1 === undefined)" js_isNull :: JSVal -> Int
foreign import javascript unsafe
  "if ($1 === undefined || $1 === null) return 0;\
   if (typeof $1 === 'boolean') return 1;\
   if (typeof $1 === 'number') return 2;\
   if (typeof $1 === 'string') return 3;\
   if (Array.isArray($1)) return 4;\
   return 5;" js_typeOf :: JSVal -> TypeResult
foreign import javascript unsafe
  "$1" js_unsafeInt :: JSVal -> Int
foreign import javascript unsafe
  "$1" js_unsafeBool :: JSVal -> Bool
foreign import javascript unsafe
  "$1" js_intJSVal :: Int -> JSVal
foreign import javascript unsafe
  "($1 ? true : false)" js_boolJSVal :: Bool -> JSVal
foreign import javascript unsafe
  "$1[$2]" js_getProp :: JSVal -> JSString -> IO JSVal
foreign import javascript unsafe
  "$1 + $2" js_concatStr :: JSString -> JSString -> JSString
foreign import javascript unsafe
  "[]" js_newEmptyArray :: IO JSVal
foreign import javascript unsafe
  "$1.push($2)" js_arrayPush :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "$1.length" js_arrayLength :: JSVal -> IO Int
foreign import javascript unsafe
  "$1[$2]" js_arrayIndex :: JSVal -> Int -> IO JSVal
#endif
