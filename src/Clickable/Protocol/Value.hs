module Clickable.Protocol.Value where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Data.Kind
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics as G
import GHC.Int

data Value
  = Vnull
  | Vbool Bool
  | Vi8 Int8
  | Vi16 Int16Le
  | Vi32 Int32Le
  | Vi64 Int64Le
  | Vu8 Word8
  | Vu16 Word16Le
  | Vu32 Word32Le
  | Vu64 Word64Le
  | Vf32 Float32Le
  | Vf64 Float64Le
  | Vstr Text
  | Varr [Value]
  | Vobj [(Text, Value)]
  | Vu8arr ByteString
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

class ToValue a where
  toValue :: a -> Value
  default toValue :: (Generic a, GToValue (Rep a)) => a -> Value
  toValue = gToValue . G.from

instance (Generic a, GToValue (Rep a)) => ToValue (Generically a) where
  toValue = gToValue . G.from . (\(Generically x) -> x)

instance ToValue Value where toValue = Prelude.id

instance ToValue Bool where toValue = Vbool

instance ToValue Int8 where toValue = Vi8
instance ToValue Int16 where toValue = Vi16 . Int16Le
instance ToValue Int32 where toValue = Vi32 . Int32Le
instance ToValue Int64 where toValue = Vi64 . Int64Le

instance ToValue Word8 where toValue = Vu8
instance ToValue Word16 where toValue = Vu16 . Word16Le
instance ToValue Word32 where toValue = Vu32 . Word32Le
instance ToValue Word64 where toValue = Vu64 . Word64Le

instance ToValue Float where toValue = Vf32 . Float32Le
instance ToValue Double where toValue = Vf64 . Float64Le

instance ToValue Int where toValue = Vi64 . Int64Le . fromIntegral

instance ToValue Char where
  toValue c = Vstr $ Text.cons c Text.empty

instance ToValue Text where toValue = Vstr

instance ToValue ByteString where toValue = Vu8arr

instance ToValue () where toValue _ = Vnull

instance ToValue a => ToValue [a] where toValue = Varr . fmap toValue

instance ToValue a => ToValue (Maybe a) where toValue = maybe Vnull toValue

instance (ToValue a, ToValue b) => ToValue (a, b) where
  toValue (a, b) = toValue [toValue a, toValue b]

instance (ToValue a, ToValue b, ToValue c) => ToValue (a, b, c) where
  toValue (a, b, c) = toValue [toValue a, toValue b, toValue c]
--------------------------------------------------------------------------------

class FromValue a where
  fromValue :: Value -> Maybe a
  default fromValue :: (Generic a, GFromValue (Rep a)) => Value -> Maybe a
  fromValue = fmap G.to . gFromValue

instance (Generic a, GFromValue (Rep a)) => FromValue (Generically a) where
  fromValue = fmap (Generically . G.to) . gFromValue

instance FromValue Value where fromValue = pure

instance FromValue Bool where
  fromValue = \case Vbool a -> Just a; _ -> Nothing

instance FromValue Int8 where
  fromValue (Vi8 j) = Just j
  fromValue _ = Nothing

instance FromValue Int16 where
  fromValue (Vi16 j) = Just j.unInt16Le
  fromValue _ = Nothing

instance FromValue Int32 where
  fromValue = \case
    Vi32 (Int32Le j) -> Just j
    Vf64 j -> Just $ floor j.unFloat64Le
    _ -> Nothing

instance FromValue Int64 where
  fromValue (Vi64 j) = Just j.unInt64Le
  fromValue _ = Nothing

instance FromValue Word8 where
  fromValue (Vu8 j) = Just j
  fromValue _ = Nothing

instance FromValue Word16 where
  fromValue (Vu16 j) = Just j.unWord16Le
  fromValue _ = Nothing

instance FromValue Word32 where
  fromValue (Vu32 j) = Just j.unWord32Le
  fromValue _ = Nothing

instance FromValue Word64 where
  fromValue (Vu64 j) = Just j.unWord64Le
  fromValue _ = Nothing

instance FromValue Float where
  fromValue (Vf32 j) = Just j.unFloat32Le
  fromValue _ = Nothing

instance FromValue Double where
  fromValue = \case
    Vi32 j -> Just $ fromIntegral j.unInt32Le
    Vf64 (Float64Le j) -> Just j
    _ -> Nothing

instance FromValue Int where
  fromValue (Vi64 j) = Just $ fromIntegral j.unInt64Le
  fromValue _ = Nothing

instance FromValue Char where
  fromValue = \case
    Vstr a | Just (c, _) <- Text.uncons a -> Just c
           | otherwise -> Nothing
    _ -> Nothing

instance FromValue Text where
  fromValue = \case Vstr a -> Just a; _ -> Nothing

instance FromValue ByteString where
  fromValue = \case Vu8arr a -> Just a; _ -> Nothing

instance FromValue () where
  fromValue = \case Vnull -> Just (); _ -> Nothing

instance FromValue a => FromValue [a] where
  fromValue = \case
    Varr xs -> Just (mapMaybe fromValue xs)
    _ -> Nothing

instance FromValue a => FromValue (Maybe a) where
  fromValue = fmap Just . fromValue @a

instance (FromValue a, FromValue b) => FromValue (a, b) where
  fromValue j = fromValue j >>= \case
    Just (a:b:_) -> (,) <$> fromValue a <*> fromValue b
    _ -> Nothing

instance (FromValue a, FromValue b, FromValue c) => FromValue (a, b, c) where
  fromValue j = fromValue j >>= \case
    Just (a:b:c:_) -> (,,) <$> fromValue a <*> fromValue b <*> fromValue c
    _ -> Nothing
--------------------------------------------------------------------------------

class GFromValue (f :: Type -> Type) where
  gFromValue :: Value -> Maybe (f a)

instance GFromValue f => GFromValue (M1 m c f) where
  gFromValue = fmap M1 . gFromValue @f

instance GFromValue U1 where
  gFromValue _ = Just U1

instance GFromJSObject (x :*: y) => GFromValue (x :*: y) where
  gFromValue (Vobj kvs) = gFromJSObject kvs
  gFromValue _ = Nothing

instance {-# OVERLAPPING #-} FromValue a => GFromValue (S1 s (Rec0 a)) where
  gFromValue = fmap (M1 . K1) . fromValue @a
--------------------------------------------------------------------------------

class GToValue (f :: Type -> Type) where
  gToValue :: f x -> Value

instance GToValue f => GToValue (M1 m c f) where
  gToValue (M1 f) = gToValue f

instance GToValue U1 where
  gToValue _ = Vnull

instance GToJSObject (x :*: y) => GToValue (x :*: y) where
  gToValue (x :*: y) = Vobj $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToValue a) => GToValue (S1 s (Rec0 a)) where
  gToValue (M1 (K1 a)) = toValue a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, Value)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToValue a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toValue a)]
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Text, Value)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromValue a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromValue
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

newtype Float64Le = Float64Le {unFloat64Le :: Double}
  deriving newtype (Show, Ord, Eq)

instance Binary Float64Le where
  put = putDoublele . unFloat64Le
  get = fmap Float64Le getDoublele

newtype Float32Le = Float32Le {unFloat32Le :: Float}
  deriving newtype (Show, Ord, Eq)

instance Binary Float32Le where
  put = putFloatle . unFloat32Le
  get = fmap Float32Le getFloatle
--------------------------------------------------------------------------------

newtype Int16Le = Int16Le {unInt16Le :: Int16}
  deriving newtype (Show, Ord, Eq)

instance Binary Int16Le where
  put = putInt16le . unInt16Le
  get = fmap Int16Le getInt16le

newtype Int32Le = Int32Le {unInt32Le :: Int32}
  deriving newtype (Show, Ord, Eq)

instance Binary Int32Le where
  put = putInt32le . unInt32Le
  get = fmap Int32Le getInt32le

newtype Int64Le = Int64Le {unInt64Le :: Int64}
  deriving newtype (Show, Ord, Eq)

instance Binary Int64Le where
  put = putInt64le . unInt64Le
  get = fmap Int64Le getInt64le
--------------------------------------------------------------------------------

newtype Word16Le = Word16Le {unWord16Le :: Word16}
  deriving newtype (Show, Ord, Eq)

instance Binary Word16Le where
  put = putWord16le . unWord16Le
  get = fmap Word16Le getWord16le

newtype Word32Le = Word32Le {unWord32Le :: Word32}
  deriving newtype (Show, Ord, Eq)

instance Binary Word32Le where
  put = putWord32le . unWord32Le
  get = fmap Word32Le getWord32le

newtype Word64Le = Word64Le {unWord64Le :: Word64}
  deriving newtype (Show, Ord, Eq)

instance Binary Word64Le where
  put = putWord64le . unWord64Le
  get = fmap Word64Le getWord64le
