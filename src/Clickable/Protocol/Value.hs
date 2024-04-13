module Clickable.Protocol.Value where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString
import Data.Kind
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics as G
import GHC.Int

data Value
  = Null
  | Bool Bool
  | I32 Int32Le
  | F64 Float64
  | String Text
  | Array [Value]
  | Object [(Text, Value)]
  | Uint8Array ByteString
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

newtype Float64 = Float64 {unFloat64 :: Double}
  deriving newtype (Show, Ord, Eq, Floating, RealFloat, Fractional, Num, RealFrac, Real)

instance Binary Float64 where
  put = putDoublele . unFloat64
  get = fmap Float64 getDoublele

newtype Int32Le = Int32Le {unInt32Le :: Int32}
  deriving newtype (Show, Ord, Eq, Num, Real, Integral, Enum, Bounded)

instance Binary Int32Le where
  put = putInt32le . unInt32Le
  get = fmap Int32Le getInt32le

class ToValue a where
  toValue :: a -> Value
  default toValue :: (Generic a, GToValue (Rep a)) => a -> Value
  toValue = gToValue . G.from

instance ToValue Value where toValue = Prelude.id

instance ToValue Bool where toValue = Bool

instance ToValue Int32 where toValue = I32 . Int32Le

instance ToValue Double where toValue = F64 . Float64

instance ToValue Char where
  toValue c = String $ Text.cons c Text.empty

instance ToValue Text where toValue = String

instance ToValue a => ToValue [a] where toValue = Array . fmap toValue

instance ToValue a => ToValue (Maybe a) where toValue = maybe Null toValue

instance (ToValue a, ToValue b) => ToValue (a, b) where
  toValue (a, b) = toValue [toValue a, toValue b]

instance (ToValue a, ToValue b, ToValue c) => ToValue (a, b, c) where
  toValue (a, b, c) = toValue [toValue a, toValue b, toValue c]
--------------------------------------------------------------------------------

class FromValue a where
  fromValue :: Value -> Maybe a
  default fromValue :: (Generic a, GFromValue (Rep a)) => Value -> Maybe a
  fromValue = fmap G.to . gFromValue

instance FromValue Value where fromValue = pure

instance FromValue Bool where
  fromValue = \case Bool a -> Just a; _ -> Nothing

instance FromValue Int32 where
  fromValue = \case
    I32 (Int32Le j) -> Just j
    F64 j -> Just $ floor j
    _ -> Nothing

instance FromValue Double where
  fromValue = \case
    I32 j -> Just $ fromIntegral j
    F64 (Float64 j) -> Just j
    _ -> Nothing

instance FromValue Char where
  fromValue = \case
    String a | Just (c, _) <- Text.uncons a -> Just c
             | otherwise -> Nothing
    _ -> Nothing

instance FromValue Text where
  fromValue = \case String a -> Just a; _ -> Nothing

instance FromValue a => FromValue [a] where
  fromValue = \case
    Array xs -> Just (mapMaybe fromValue xs)
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

instance GFromJSObject (x :*: y) => GFromValue (x :*: y) where
  gFromValue (Object kvs) = gFromJSObject kvs
  gFromValue _ = Nothing

instance {-# OVERLAPPING #-} FromValue a => GFromValue (S1 s (Rec0 a)) where
  gFromValue = fmap (M1 . K1) . fromValue @a
--------------------------------------------------------------------------------

class GToValue (f :: Type -> Type) where
  gToValue :: f x -> Value

instance GToValue f => GToValue (M1 m c f) where
  gToValue (M1 f) = gToValue f

instance GToJSObject (x :*: y) => GToValue (x :*: y) where
  gToValue (x :*: y) = Object $ gToJSObject (x :*: y)

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
