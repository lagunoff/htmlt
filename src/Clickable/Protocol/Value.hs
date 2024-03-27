module Clickable.Protocol.Value where

import Data.Binary
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
  | I64 Int64
  | F64 Double
  | String Text
  | Array [Value]
  | Object [(Text, Value)]
  | Uint8Array ByteString
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

class ToJSValue a where
  toJSValue :: a -> Value
  default toJSValue :: (Generic a, GToJSValue (Rep a)) => a -> Value
  toJSValue = gToJSValue . G.from

instance ToJSValue Value where toJSValue = Prelude.id

instance ToJSValue Bool where toJSValue = Bool

instance ToJSValue Int64 where toJSValue = I64

instance ToJSValue Double where toJSValue = F64

instance ToJSValue Char where
  toJSValue c = String $ Text.cons c Text.empty

instance ToJSValue Text where toJSValue = String

instance ToJSValue a => ToJSValue [a] where toJSValue = Array . fmap toJSValue

instance ToJSValue a => ToJSValue (Maybe a) where toJSValue = maybe Null toJSValue

instance (ToJSValue a, ToJSValue b) => ToJSValue (a, b) where
  toJSValue (a, b) = toJSValue [toJSValue a, toJSValue b]

instance (ToJSValue a, ToJSValue b, ToJSValue c) => ToJSValue (a, b, c) where
  toJSValue (a, b, c) = toJSValue [toJSValue a, toJSValue b, toJSValue c]
--------------------------------------------------------------------------------

class FromJSValue a where
  fromJSValue :: Value -> Maybe a
  default fromJSValue :: (Generic a, GFromJSValue (Rep a)) => Value -> Maybe a
  fromJSValue = fmap G.to . gFromJSValue

instance FromJSValue Value where fromJSValue = pure

instance FromJSValue Bool where
  fromJSValue = \case Bool a -> Just a; _ -> Nothing

instance FromJSValue Int64 where
  fromJSValue = \case
    I64 j -> Just j
    F64 j -> Just $ floor j
    _ -> Nothing

instance FromJSValue Double where
  fromJSValue = \case
    I64 j -> Just $ fromIntegral j
    F64 j -> Just j
    _ -> Nothing

instance FromJSValue Char where
  fromJSValue = \case
    String a | Just (c, _) <- Text.uncons a -> Just c
             | otherwise -> Nothing
    _ -> Nothing

instance FromJSValue Text where
  fromJSValue = \case String a -> Just a; _ -> Nothing

instance FromJSValue a => FromJSValue [a] where
  fromJSValue = \case
    Array xs -> Just (mapMaybe fromJSValue xs)
    _ -> Nothing

instance FromJSValue a => FromJSValue (Maybe a) where
  fromJSValue = fmap Just . fromJSValue @a

instance (FromJSValue a, FromJSValue b) => FromJSValue (a, b) where
  fromJSValue j = fromJSValue j >>= \case
    Just (a:b:_) -> (,) <$> fromJSValue a <*> fromJSValue b
    _ -> Nothing

instance (FromJSValue a, FromJSValue b, FromJSValue c) => FromJSValue (a, b, c) where
  fromJSValue j = fromJSValue j >>= \case
    Just (a:b:c:_) -> (,,) <$> fromJSValue a <*> fromJSValue b <*> fromJSValue c
    _ -> Nothing
--------------------------------------------------------------------------------

class GFromJSValue (f :: Type -> Type) where
  gFromJSValue :: Value -> Maybe (f a)

instance GFromJSValue f => GFromJSValue (M1 m c f) where
  gFromJSValue = fmap M1 . gFromJSValue @f

instance GFromJSObject (x :*: y) => GFromJSValue (x :*: y) where
  gFromJSValue (Object kvs) = gFromJSObject kvs
  gFromJSValue _ = Nothing

instance {-# OVERLAPPING #-} FromJSValue a => GFromJSValue (S1 s (Rec0 a)) where
  gFromJSValue = fmap (M1 . K1) . fromJSValue @a
--------------------------------------------------------------------------------

class GToJSValue (f :: Type -> Type) where
  gToJSValue :: f x -> Value

instance GToJSValue f => GToJSValue (M1 m c f) where
  gToJSValue (M1 f) = gToJSValue f

instance GToJSObject (x :*: y) => GToJSValue (x :*: y) where
  gToJSValue (x :*: y) = Object $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToJSValue a) => GToJSValue (S1 s (Rec0 a)) where
  gToJSValue (M1 (K1 a)) = toJSValue a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, Value)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToJSValue a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toJSValue a)]
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Text, Value)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSValue a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromJSValue
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
