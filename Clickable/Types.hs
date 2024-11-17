{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}
module Clickable.Types where

import Clickable.Binary
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Binary ( Binary )
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Word
import GHC.Exts
import GHC.Generics qualified as G
import GHC.List qualified as List
import GHC.Types
import GHC.Generics

newtype JSM a = JSM {unJSM :: InternalEnv -> IO a}
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadReader InternalEnv
  ) via ReaderT InternalEnv IO

instance MonadState InternalState JSM where
  state f = JSM \e -> atomicModifyIORef' e.ien_state (swap . f)
  {-# INLINE state #-}
  get = JSM \e -> readIORef e.ien_state
  {-# INLINE get #-}
  put s = JSM \e -> writeIORef e.ien_state s
  {-# INLINE put #-}

class MonadJSM m where
  liftJSM :: JSM a -> m a

data InternalEnv = InternalEnv {
  ien_command :: JSExp -> IO (),
  ien_flush :: IO JSVal,
  ien_state :: IORef InternalState,
  ien_scope :: ScopeId,
  ien_prompt_tag :: PromptTag (),
  ien_continuations :: IORef (Map ContId (IO JSVal -> IO ()))
}

data InternalState = InternalState {
  ist_subscriptions :: Map EventId [Subscription Any],
  ist_resources :: Map ScopeId Resources,
  ist_transaction_queue :: Map EventId (JSM ()),
  ist_id_supply :: Word32
}

data Subscription a = Subscription {
  sub_scope :: ScopeId,
  sub_callback :: a -> JSM ()
}

data Resources = Resources {
  rsr_parent :: ScopeId,
  rsr_linked :: [ScopeId],
  rsr_finalizers :: [JSM ()]
}

newtype HTML a = HTML {unHTML :: Maybe RefId -> InternalEnv -> IO (a, Maybe RefId)}
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadIO
  ) via StateT (Maybe RefId) JSM

instance MonadJSM HTML where
  liftJSM (JSM a) = HTML \s e -> (,s) <$> a e
  {-# INLINE liftJSM #-}

data JSExp where
  Null :: JSExp
  Bool :: Word8 -> JSExp
  I8 :: Int8 -> JSExp
  I16 :: Int16 -> JSExp
  I32 :: Int32 -> JSExp
  I64 :: Int64 -> JSExp
  U8 :: Word8 -> JSExp
  U16 :: Word16 -> JSExp
  U32 :: Word32 -> JSExp
  U64 :: Word64 -> JSExp
  F32 :: Float32 -> JSExp
  F64 :: Float64 -> JSExp
  Str :: Text -> JSExp
  Arr :: [JSExp] -> JSExp
  Obj :: [(Text, JSExp)] -> JSExp -- ^ JavaScript object
  U8Arr :: ByteString -> JSExp

  Dot :: JSExp -> Text -> JSExp
  SetProp :: JSExp -> Text -> JSExp -> JSExp
  Ix :: JSExp -> Word32 -> JSExp
  Id :: Text -> JSExp

  Lam :: JSExp -> JSExp
  Arg :: Word8 -> JSExp
  Apply :: JSExp -> [JSExp] -> JSExp
  Call :: JSExp -> Text -> [JSExp] -> JSExp

  AssignRef :: RefId -> JSExp -> JSExp
  FreeRef :: RefId -> JSExp
  Ref :: RefId -> JSExp
  FreeScope :: ScopeId -> JSExp

  PeekStack :: Word8 -> JSExp
  PushStack :: JSExp -> JSExp
  PopStack :: JSExp

  PopIns :: JSExp
  ElementProp :: JSExp -> Text -> JSExp -> JSExp
  ElementAttr :: JSExp -> Text -> Text -> JSExp
  ClassListAdd :: JSExp -> Text -> JSExp
  ClassListRemove :: JSExp -> Text -> JSExp
  InsertBrackets :: JSExp
  ClearBrackets :: JSExp -> JSExp
  DetachBrackets :: JSExp -> JSExp

  CreateElement :: Text -> JSExp
  CreateElementNS :: Text -> Text -> JSExp
  CreateText :: Text -> JSExp
  UpdateText :: JSExp -> Text -> JSExp

  Eval :: UnsafeJavaScript -> JSExp
  TriggerEvent :: EventId -> JSExp -> JSExp
  Resume :: ContId -> JSExp

  deriving stock (Generic, Show)
  deriving anyclass Binary

data ClientMsg where
  StartMsg :: StartFlags -> ClientMsg
  EventMsg :: EventId -> JSExp -> ClientMsg
  ResumeMsg :: ContId -> JSExp -> ClientMsg
  deriving stock Generic
  deriving anyclass Binary

-- | JavaScript value, result of evaluating an 'JSExp'. Should only
-- contain constructors up to and including 'U8Arr'. I opted for type
-- alias rather than a datatype for easier conversion.
type JSVal = JSExp

newtype StartFlags = StartFlags {unStartFlags :: JSVal}
  deriving newtype Binary

newtype ScopeId = ScopeId {unScopeId :: Word32}
  deriving newtype (Binary, Eq, Ord, Show)

data RefId = RefId ScopeId Word32
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

newtype EventId = EventId {unEventId :: Word32}
  deriving newtype (Show, Ord, Eq, Binary)

newtype ContId = ContId {unContId :: Word32}
  deriving newtype (Show, Ord, Eq, Binary)

newtype UnsafeJavaScript = UnsafeJavaScript {unUnsafeJavaScript :: Text}
  deriving newtype (IsString, Show, Semigroup, Monoid, Binary)

newtype Event a = Event {unEvent :: EventId}
  deriving newtype (Show, Ord, Eq, Binary)

unsafeFromEventId :: EventId -> Event a
unsafeFromEventId = Event

unsafeToEventId :: Event a -> EventId
unsafeToEventId = unEvent

data DynVar a where
  SourceVar :: Event a -> IORef a -> DynVar a
  OverrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
  LensMap :: Lens' s a -> DynVar s -> DynVar a

type UpdateFn s = forall a. (s -> (s, a)) -> JSM a

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

data Dynamic a where
  ConstVal :: a -> Dynamic a
  FromVar :: DynVar a -> Dynamic a
  MapVal :: Dynamic a -> (a -> b) -> Dynamic b
  SplatVal :: Dynamic (a -> b) -> Dynamic a -> Dynamic b
  OverrideSub :: (forall b. SubscribeFn a b -> SubscribeFn a b) -> Dynamic a -> Dynamic a

type SubscribeFn a b = (a -> b -> JSM b) -> JSM ()

instance Functor Dynamic where
  fmap = flip MapVal
  {-# INLINE fmap #-}
instance Applicative Dynamic where
  pure = ConstVal
  {-# INLINE pure #-}
  (<*>) = SplatVal
  {-# INLINE (<*>) #-}

fromVar :: DynVar a -> Dynamic a
fromVar = FromVar
{-# INLINE fromVar #-}

class ToJSVal a where
  toJSVal :: a -> JSVal
  default toJSVal :: (Generic a, GToJSVal (Rep a)) => a -> JSVal
  toJSVal = gToJSVal . G.from

instance (Generic a, GToJSVal (Rep a)) => ToJSVal (Generically a) where
  toJSVal = gToJSVal . G.from . (\(Generically x) -> x)

instance ToJSVal JSVal where toJSVal = Prelude.id

instance ToJSVal Bool where toJSVal = Bool . bool 0 1

instance ToJSVal Int8 where toJSVal = I8
instance ToJSVal Int16 where toJSVal = I16
instance ToJSVal Int32 where toJSVal = I32
instance ToJSVal Int64 where toJSVal = I64

instance ToJSVal Word8 where toJSVal = U8
instance ToJSVal Word16 where toJSVal = U16
instance ToJSVal Word32 where toJSVal = U32
instance ToJSVal Word64 where toJSVal = U64

instance ToJSVal Float where toJSVal = F32 . Float32
instance ToJSVal Double where toJSVal = F64 . Float64

instance ToJSVal Int where toJSVal = I64 . fromIntegral
instance ToJSVal Word where toJSVal = U64 . fromIntegral

instance ToJSVal Char where
  toJSVal c = Str $ Text.cons c Text.empty

instance ToJSVal Text where toJSVal = Str

instance ToJSVal ByteString where toJSVal = U8Arr

instance ToJSVal () where toJSVal _ = Null

instance ToJSVal a => ToJSVal [a] where toJSVal = Arr . fmap toJSVal

instance ToJSVal a => ToJSVal (Maybe a) where toJSVal = maybe Null toJSVal

instance (ToJSVal a, ToJSVal b) => ToJSVal (a, b) where
  toJSVal (a, b) = toJSVal [toJSVal a, toJSVal b]

instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a, b, c) where
  toJSVal (a, b, c) = toJSVal [toJSVal a, toJSVal b, toJSVal c]
--------------------------------------------------------------------------------

class FromJSVal a where
  fromJSVal :: JSVal -> Maybe a
  default fromJSVal :: (Generic a, GFromJSVal (Rep a)) => JSVal -> Maybe a
  fromJSVal = fmap G.to . gFromJSVal

instance (Generic a, GFromJSVal (Rep a)) => FromJSVal (Generically a) where
  fromJSVal = fmap (Generically . G.to) . gFromJSVal

instance FromJSVal JSVal where fromJSVal = pure

instance FromJSVal Bool where
  fromJSVal (Bool 0) = Just False
  fromJSVal (Bool _) = Just True
  fromJSVal _ = Nothing

instance FromJSVal Int8 where
  fromJSVal (I8 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Int16 where
  fromJSVal (I16 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Int32 where
  fromJSVal (I32 j) = Just j
  fromJSVal (F64 j) = Just $ floor j.unFloat64
  fromJSVal _ = Nothing

instance FromJSVal Int64 where
  fromJSVal (I64 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Word8 where
  fromJSVal (U8 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Word16 where
  fromJSVal (U16 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Word32 where
  fromJSVal (U32 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Word64 where
  fromJSVal (U64 j) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Float where
  fromJSVal (F32 j) = Just j.unFloat32
  fromJSVal _ = Nothing

instance FromJSVal Double where
  fromJSVal (I32 j) = Just $ fromIntegral j
  fromJSVal (F64 (Float64 j)) = Just j
  fromJSVal _ = Nothing

instance FromJSVal Int where
  fromJSVal (I8 j) = Just $ fromIntegral j
  fromJSVal (I16 j) = Just $ fromIntegral j
  fromJSVal (I32 j) = Just $ fromIntegral j
  fromJSVal (I64 j) = Just $ fromIntegral j
  fromJSVal (U8 j) = Just $ fromIntegral j
  fromJSVal (U16 j) = Just $ fromIntegral j
  fromJSVal (U32 j) = Just $ fromIntegral j
  fromJSVal (U64 j) = Just $ fromIntegral j
  fromJSVal _ = Nothing

instance FromJSVal Word where
  fromJSVal (I8 j) = Just $ fromIntegral j
  fromJSVal (I16 j) = Just $ fromIntegral j
  fromJSVal (I32 j) = Just $ fromIntegral j
  fromJSVal (I64 j) = Just $ fromIntegral j
  fromJSVal (U8 j) = Just $ fromIntegral j
  fromJSVal (U16 j) = Just $ fromIntegral j
  fromJSVal (U32 j) = Just $ fromIntegral j
  fromJSVal (U64 j) = Just $ fromIntegral j
  fromJSVal _ = Nothing

instance FromJSVal Char where
  fromJSVal = \case
    Str a | Just (c, _) <- Text.uncons a -> Just c
          | otherwise -> Nothing
    _ -> Nothing

instance FromJSVal Text where
  fromJSVal = \case Str a -> Just a; _ -> Nothing

instance FromJSVal ByteString where
  fromJSVal = \case U8Arr a -> Just a; _ -> Nothing

instance FromJSVal () where
  fromJSVal = \case Null -> Just (); _ -> Nothing

instance FromJSVal a => FromJSVal [a] where
  fromJSVal = \case
    Arr xs -> Just (mapMaybe fromJSVal xs)
    _ -> Nothing

instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal = fmap Just . fromJSVal @a

instance (FromJSVal a, FromJSVal b) => FromJSVal (a, b) where
  fromJSVal j = fromJSVal j >>= \case
    Just (a:b:_) -> (,) <$> fromJSVal a <*> fromJSVal b
    _ -> Nothing

instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a, b, c) where
  fromJSVal j = fromJSVal j >>= \case
    Just (a:b:c:_) -> (,,) <$> fromJSVal a <*> fromJSVal b <*> fromJSVal c
    _ -> Nothing
--------------------------------------------------------------------------------

class GFromJSVal (f :: Type -> Type) where
  gFromJSVal :: JSVal -> Maybe (f a)

instance GFromJSVal f => GFromJSVal (M1 m c f) where
  gFromJSVal = fmap M1 . gFromJSVal @f

instance GFromJSVal U1 where
  gFromJSVal _ = Just U1

instance GFromJSObject (x :*: y) => GFromJSVal (x :*: y) where
  gFromJSVal (Obj kvs) = gFromJSObject kvs
  gFromJSVal _ = Nothing

instance GFromJSSum (x :+: y) => GFromJSVal (x :+: y) where
  gFromJSVal (Arr [Str tag, v]) = gFromJSSum tag v
  gFromJSVal _ = Nothing

instance {-# OVERLAPPING #-} FromJSVal a => GFromJSVal (S1 s (Rec0 a)) where
  gFromJSVal = fmap (M1 . K1) . fromJSVal @a
--------------------------------------------------------------------------------

class GToJSVal (f :: Type -> Type) where
  gToJSVal :: f x -> JSVal

instance GToJSVal f => GToJSVal (M1 m c f) where
  gToJSVal (M1 f) = gToJSVal f

instance GToJSVal U1 where
  gToJSVal _ = Null

instance GToJSObject (x :*: y) => GToJSVal (x :*: y) where
  gToJSVal (x :*: y) = Obj $ gToJSObject (x :*: y)

instance GToJSSum (x :+: y) => GToJSVal (x :+: y) where
  gToJSVal = gToJSSum

instance {-# OVERLAPPING #-} (ToJSVal a) => GToJSVal (S1 s (Rec0 a)) where
  gToJSVal (M1 (K1 a)) = toJSVal a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, JSVal)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToJSVal a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toJSVal a)]
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Text, JSVal)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSVal a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromJSVal
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSSum (f :: Type -> Type) where
  gFromJSSum :: Text -> JSVal -> Maybe (f x)

instance (GFromJSSum x, GFromJSSum y) => GFromJSSum (x :+: y) where
  gFromJSSum tag v = case gFromJSSum @x tag v of
    Just a -> Just $ L1 a
    Nothing -> case gFromJSSum @y tag v of
      Just b -> Just $ R1 b
      Nothing -> Nothing

instance FromJSVal a => GFromJSVal (K1 R a) where
  gFromJSVal v = fmap K1 $ fromJSVal @a v

instance {-# OVERLAPPING #-} (GFromJSVal (f a), Constructor s) => GFromJSSum (C1 s (f a)) where
  gFromJSSum tag v =
    if tag == key then fmap M1 $ gFromJSVal v else Nothing
    where
      key = Text.pack $ conName (undefined :: C1 s (f a) x)

instance {-# OVERLAPPING #-} Constructor s => GFromJSSum (C1 s U1) where
  gFromJSSum tag _ =
    if tag == key then Just (M1 U1) else Nothing
    where
      key = Text.pack $ conName (undefined :: C1 s (f a) x)
--------------------------------------------------------------------------------

class GToJSSum (f :: Type -> Type) where
  gToJSSum :: f x -> JSVal

instance (GToJSSum x, GToJSSum y) => GToJSSum (x :+: y) where
  gToJSSum (L1 v) = gToJSSum v
  gToJSSum (R1 v) = gToJSSum v

instance ToJSVal a => GToJSVal (K1 R a) where
  gToJSVal (K1 v) = toJSVal @a v

instance {-# OVERLAPPING #-} (GToJSVal (f a), Constructor s) => GToJSSum (C1 s (f a)) where
  gToJSSum (M1 v) =
    Arr [Str key, gToJSVal v]
    where
      key = Text.pack $ conName (undefined :: C1 s (f a) x)

instance {-# OVERLAPPING #-} Constructor s => GToJSSum (C1 s U1) where
  gToJSSum (M1 U1) = Arr [Str key] where
    key = Text.pack $ conName (undefined :: C1 s (f a) x)
--------------------------------------------------------------------------------

{-| Boxed versions of [Delimited
Continuation](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst)
primops. If you are not familiar with the concept, I highly recommend
watching [Delimited Continuations, Demystified by Alexis
King](https://www.youtube.com/watch?v=TE48LsgVlIU).

TODO: Remove these once they are included in 'base' -}

prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag t) (IO m) = IO (prompt# t m)

control :: forall a b. PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control (PromptTag t) f = IO (control0# t g)
  where
    g :: ((State# RealWorld -> (# State# RealWorld, b #))
      -> State# RealWorld -> (# State# RealWorld, a #))
      -> State# RealWorld -> (# State# RealWorld, a #)
    g h = let IO m = f (k h) in m
    k :: ((State# RealWorld -> (# State# RealWorld, b #))
      -> State# RealWorld -> (# State# RealWorld, a #))
      -> IO b -> IO a
    k l (IO n) = IO (l n)

data PromptTag a = PromptTag {unPromptTag :: PromptTag# a}

newPromptTag :: forall a. IO (PromptTag a)
newPromptTag = IO \s ->
  let !(# s', t #) = newPromptTag# @a s in (# s', PromptTag t #)
