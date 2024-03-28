{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Wasm.Compat.Marshal where

import Control.Monad
import Data.Maybe
import Data.String
import Data.Text qualified as Text
import Data.List qualified as List
import Data.Coerce
import Data.Kind
import Wasm.Compat.Prim
import GHC.Prim
import GHC.Ptr
import GHC.Int
import GHC.Generics as G
import GHC.IO
import Data.Text.Internal
import Data.Array.Byte
import Data.Word

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
--------------------------------------------------------------------------------

instance IsString JSString where fromString = toJSString

instance Semigroup JSString where (<>) = js_concatStr

instance Monoid JSString where mempty = js_emptyStr

instance Eq JSString where (==) a b = fromJSString a == fromJSString b

instance Show JSString where show = show . fromJSString

instance Ord JSString where compare a b = fromJSString a `compare` fromJSString b
--------------------------------------------------------------------------------

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  default fromJSVal :: (Generic a, GFromJSVal (Rep a)) => JSVal -> IO (Maybe a)
  fromJSVal = fmap (fmap G.to) . gFromJSVal

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

instance FromJSVal v => FromJSVal (Maybe v) where
  fromJSVal j = maybe (pure (Just Nothing)) fromJSVal $
    nullableToMaybe (Nullable j)

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

instance FromJSVal Text where
  fromJSVal j = case js_typeOf j of
    TypeString -> fmap Just $ textFromJSString $ JSString j
    _ -> pure Nothing

instance (FromJSVal a, FromJSVal b) => FromJSVal (a, b) where
  fromJSVal j = do
    ma <- fromJSVal =<< js_arrayIndex j 0
    mb <- fromJSVal =<< js_arrayIndex j 1
    return $ liftA2 (,) ma mb

instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a, b, c) where
  fromJSVal j = do
    ma <- fromJSVal =<< js_arrayIndex j 0
    mb <- fromJSVal =<< js_arrayIndex j 1
    mc <- fromJSVal =<< js_arrayIndex j 2
    return $ (,,) <$> ma <*> mb <*> mc

--------------------------------------------------------------------------------
class ToJSVal a where
  toJSVal :: a -> IO JSVal
  default toJSVal :: (Generic a, GToJSVal (Rep a)) => a -> IO JSVal
  toJSVal = gToJSVal . G.from

class ToJSValPure v where toJSValPure :: v -> JSVal

instance {-# OVERLAPS #-} ToJSValPure v => ToJSVal v where
  toJSVal = pure . toJSValPure

instance ToJSValPure Int where toJSValPure = js_intJSVal

instance ToJSValPure Bool where toJSValPure = js_boolJSVal

instance ToJSValPure JSVal where toJSValPure j = j

instance ToJSValPure JSString where toJSValPure (JSString j) = j

instance ToJSVal v => ToJSVal (Maybe v) where
  toJSVal = fmap (unNullable . maybeToNullable) . mapM toJSVal

instance ToJSValPure v => ToJSValPure (Maybe v) where
  toJSValPure = unNullable . maybeToNullable . fmap toJSValPure

instance ToJSVal v => ToJSVal [v] where
  toJSVal s = do
    arr <- js_newEmptyArray
    forM_ s $ toJSVal >=> js_arrayPush arr
    return arr

instance ToJSVal Text where
  toJSVal = fmap (\(JSString j) -> j) . textToJSString

instance (ToJSVal a, ToJSVal b) => ToJSVal (a, b) where
  toJSVal (a, b) = do
    ja <- toJSVal a
    jb <- toJSVal b
    toJSVal [ja, jb]

instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a, b, c) where
  toJSVal (a, b, c) = do
    ja <- toJSVal a
    jb <- toJSVal b
    jc <- toJSVal c
    toJSVal [ja, jb, jc]

--------------------------------------------------------------------------------

class GFromJSVal (f :: Type -> Type) where
  gFromJSVal :: JSVal -> IO (Maybe (f a))

instance GFromJSVal f => GFromJSVal (M1 m c f) where
  gFromJSVal = fmap (fmap M1) . gFromJSVal @f

instance GFromJSObject (x :*: y) => GFromJSVal (x :*: y) where
  gFromJSVal kvs = gFromJSObject kvs

instance {-# OVERLAPPING #-} FromJSVal a => GFromJSVal (S1 s (Rec0 a)) where
  gFromJSVal = fmap (fmap (M1 . K1)) . fromJSVal @a
--------------------------------------------------------------------------------

class GToJSVal (f :: Type -> Type) where
  gToJSVal :: f x -> IO JSVal

instance GToJSVal f => GToJSVal (M1 m c f) where
  gToJSVal (M1 f) = gToJSVal f

instance GToJSObject (x :*: y) => GToJSVal (x :*: y) where
  gToJSVal (x :*: y) = do
    o <- js_newObject
    gToJSObject (x :*: y) o
    return o

instance {-# OVERLAPPING #-} (ToJSVal a) => GToJSVal (S1 s (Rec0 a)) where
  gToJSVal (M1 (K1 a)) = toJSVal a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> JSVal -> IO ()

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) o = gToJSObject x o >> gToJSObject y o

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) o = gToJSObject a o

instance {-# OVERLAPPING #-} (ToJSVal a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) o = do
    v <- toJSVal a
    js_assignProp o addr len v
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
      !(Text (ByteArray arr) off len) = key
      addr = Ptr (byteArrayContents# arr) `plusPtr` off

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: JSVal -> IO (Maybe (f x))

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = do
    x <- gFromJSObject kvs
    y <- gFromJSObject kvs
    return $ liftA2 (:*:) x y

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap (fmap M1) . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSVal a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = js_getProp kvs addr len >>= fmap (fmap (M1 . K1)) . fromJSVal
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
      !(Text (ByteArray arr) off len) = key
      addr = Ptr (byteArrayContents# arr) `plusPtr` off
--------------------------------------------------------------------------------

textToJSString :: Text -> IO JSString
textToJSString (Text (ByteArray arr) off len) = do
  let addr = byteArrayContents# arr
  js_decodeUtf8 (Ptr addr `plusPtr` off) len

textFromJSString :: JSString -> IO Text
textFromJSString j = IO \s0 ->
  let (# s1, len@(I# len#) #) = unIO (js_stringLength j) s0
      (# s2, marr #) = newByteArray# (len# *# 3#) s1
      (# s3, tlen #) = unIO (js_encodeUtf8 j (Ptr (mutableByteArrayContents# marr)) len) s2
      (# s4, arr #) = unsafeFreezeByteArray# marr s3
  in  (# s4, (Text (ByteArray arr) 0 tlen) #)

newtype UnsafeJavaScript = UnsafeJavaScript {unUnsafeJavaScript :: Text}
  deriving newtype (IsString, Semigroup, Monoid)

evalJavaScript :: UnsafeJavaScript -> IO JSVal
evalJavaScript rjs = do
  let Text (ByteArray arr) off len = rjs.unUnsafeJavaScript
      addr = byteArrayContents# arr
  js_evalJavaScript (Ptr addr `plusPtr` off) len

evalJavaScript1 :: ToJSVal arg0 => arg0 -> UnsafeJavaScript -> IO JSVal
evalJavaScript1 arg0 rjs = do
  a0 <- toJSVal arg0
  let Text (ByteArray arr) off len = rjs.unUnsafeJavaScript
      addr = byteArrayContents# arr
  js_evalJavaScript1 a0 (Ptr addr `plusPtr` off) len

evalJavaScript2 :: (ToJSVal arg0, ToJSVal arg1) => arg0 -> arg1 -> UnsafeJavaScript -> IO JSVal
evalJavaScript2 arg0 arg1 rjs = do
  a0 <- toJSVal arg0
  a1 <- toJSVal arg1
  let Text (ByteArray arr) off len = rjs.unUnsafeJavaScript
      addr = byteArrayContents# arr
  js_evalJavaScript2 a0 a1 (Ptr addr `plusPtr` off) len

evalJavaScript3 :: (ToJSVal arg0, ToJSVal arg1, ToJSVal arg2) => arg0 -> arg1 -> arg2 -> UnsafeJavaScript -> IO JSVal
evalJavaScript3 arg0 arg1 arg2 rjs = do
  a0 <- toJSVal arg0
  a1 <- toJSVal arg1
  a2 <- toJSVal arg2
  let Text (ByteArray arr) off len = rjs.unUnsafeJavaScript
      addr = byteArrayContents# arr
  js_evalJavaScript3 a0 a1 a2 (Ptr addr `plusPtr` off) len

evalJavaScript4 :: (ToJSVal arg0, ToJSVal arg1, ToJSVal arg2, ToJSVal arg3) => arg0 -> arg1 -> arg2 -> arg3 -> UnsafeJavaScript -> IO JSVal
evalJavaScript4 arg0 arg1 arg2 arg3 rjs = do
  a0 <- toJSVal arg0
  a1 <- toJSVal arg1
  a2 <- toJSVal arg2
  a3 <- toJSVal arg3
  let Text (ByteArray arr) off len = rjs.unUnsafeJavaScript
      addr = byteArrayContents# arr
  js_evalJavaScript4 a0 a1 a2 a3 (Ptr addr `plusPtr` off) len

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
js_concatStr :: JSString -> JSString -> JSString = undefined
js_newEmptyArray :: IO JSVal = undefined
js_arrayPush :: JSVal -> JSVal -> IO () = undefined
js_arrayLength :: JSVal -> IO Int = undefined
js_arrayIndex :: JSVal -> Int -> IO JSVal = undefined
js_decodeUtf8 :: Ptr Word8 -> Int -> IO JSString = undefined
js_encodeUtf8 :: JSString -> Ptr Word8 -> Int -> IO Int = undefined
js_stringLength :: JSString -> IO Int = undefined
js_evalJavaScript :: Ptr Word8 -> Int -> IO JSVal = undefined
js_evalJavaScript1 :: JSVal -> Ptr Word8 -> Int -> IO JSVal = undefined
js_evalJavaScript2 :: JSVal -> JSVal -> Ptr Word8 -> Int -> IO JSVal = undefined
js_evalJavaScript3 :: JSVal -> JSVal -> JSVal -> Ptr Word8 -> Int -> IO JSVal = undefined
js_evalJavaScript4 :: JSVal -> JSVal -> JSVal -> JSVal -> Ptr Word8 -> Int -> IO JSVal = undefined
js_getProp :: JSVal -> Ptr Word8 -> Int -> IO JSVal = undefined
js_assignProp :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO () = undefined
js_newObject :: IO JSVal = undefined

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
  "$1 + $2" js_concatStr :: JSString -> JSString -> JSString
foreign import javascript unsafe
  "[]" js_newEmptyArray :: IO JSVal
foreign import javascript unsafe
  "$1.push($2)" js_arrayPush :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "$1.length" js_arrayLength :: JSVal -> IO Int
foreign import javascript unsafe
  "$1[$2]" js_arrayIndex :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe
  "(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $1, $2)))"
  js_decodeUtf8 :: Ptr Word8 -> Int -> IO JSString
foreign import javascript unsafe
  "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encodeUtf8 :: JSString -> Ptr Word8 -> Int -> IO Int
foreign import javascript unsafe
  "$1.length"
  js_stringLength :: JSString -> IO Int
foreign import javascript unsafe
  "eval(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $1, $2)))"
  js_evalJavaScript :: Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "eval(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3)))($1)"
  js_evalJavaScript1 :: JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "eval(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $3, $4)))($1, $2)"
  js_evalJavaScript2 :: JSVal -> JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "eval(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $4, $5)))($1, $2, $3)"
  js_evalJavaScript3 :: JSVal -> JSVal -> JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "eval(new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $5, $6)))($1, $2, $3, $4)"
  js_evalJavaScript4 :: JSVal -> JSVal -> JSVal -> JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "$1[new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3))]"
  js_getProp :: JSVal -> Ptr Word8 -> Int -> IO JSVal
foreign import javascript unsafe
  "$1[new TextDecoder('utf8').decode(new Uint8Array(__exports.memory.buffer, $2, $3))] = $4;"
  js_assignProp :: JSVal -> Ptr Word8 -> Int -> JSVal -> IO ()
foreign import javascript unsafe
  "{}"
  js_newObject :: IO JSVal
#endif
