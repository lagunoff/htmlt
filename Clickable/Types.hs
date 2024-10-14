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

import Clickable.Float
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Binary ( Binary )
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Builder ( Builder )
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

newtype ClickM a = ClickM {unClickM :: InternalEnv -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader InternalEnv)
    via ReaderT InternalEnv IO

instance MonadState InternalState ClickM where
  state f = ClickM \e ->
    atomicModifyIORef' e.hte_state (swap . f)
  {-# INLINE state #-}
  get = ClickM \e -> readIORef e.hte_state
  {-# INLINE get #-}
  put s = ClickM \e -> writeIORef e.hte_state s
  {-# INLINE put #-}

data InternalEnv = InternalEnv
  { hte_send :: Builder -> IO ()
  , hte_flush :: IO ValueExpr
  , hte_state :: IORef InternalState
  , hte_scope :: ScopeId
  , hte_prompt_tag :: PromptTag ()
  }

data InternalState = InternalState
  { subscriptions :: [Subscription Any]
  , finalizers :: [Finalizer]
  , transaction_queue :: Map EventId (ClickM ())
  , next_id :: Word32
  }

newtype HtmlM a = HtmlM
  {unHtmlM :: Maybe RefId -> InternalEnv -> IO (a, Maybe RefId)}
  deriving (Functor, Applicative, Monad, MonadIO) via StateT (Maybe RefId) ClickM

liftClick :: ClickM a -> HtmlM a
liftClick (ClickM a) = HtmlM \s e -> (,s) <$> a e

data Expr where
  Null :: Expr
  Bool :: Word8 -> Expr
  I8 :: Int8 -> Expr
  I16 :: Int16 -> Expr
  I32 :: Int32 -> Expr
  I64 :: Int64 -> Expr
  U8 :: Word8 -> Expr
  U16 :: Word16 -> Expr
  U32 :: Word32 -> Expr
  U64 :: Word64 -> Expr
  F32 :: Float32 -> Expr
  F64 :: Float64 -> Expr
  Str :: Text -> Expr
  Arr :: [Expr] -> Expr
  Obj :: [(Text, Expr)] -> Expr -- ^ JavaScript object
  U8Arr :: ByteString -> Expr

  Dot :: Expr -> Text -> Expr
  SetProp :: Expr -> Text -> Expr -> Expr
  Ix :: Expr -> Word32 -> Expr
  Id :: Text -> Expr

  Lam :: Expr -> Expr
  Arg :: Word8 -> Expr
  Apply :: Expr -> [Expr] -> Expr
  Call :: Expr -> Text -> [Expr] -> Expr

  AssignRef :: RefId -> Expr -> Expr
  FreeRef :: RefId -> Expr
  Ref :: RefId -> Expr
  FreeScope :: ScopeId -> Expr

  PeekStack :: Word8 -> Expr
  PushStack :: Expr -> Expr
  PopStack :: Expr

  PopIns :: Expr
  ElementProp :: Expr -> Text -> Expr -> Expr
  ElementAttr :: Expr -> Text -> Text -> Expr
  ClassListAdd :: [Text] -> Expr
  ClassListRemove :: [Text] -> Expr
  InsertBrackets :: Expr
  ClearBrackets :: Expr -> Expr
  DropBrackets :: Expr -> Expr

  CreateElement :: Text -> Expr
  CreateElementNS :: Text -> Text -> Expr
  CreateTextNode :: Text -> Expr
  UpdateTextNode :: Expr -> Text -> Expr

  Eval :: UnsafeJavaScript -> Expr
  TriggerEvent :: EventId -> Expr -> Expr
  YieldResult :: Word32 -> Expr

  deriving stock Generic
  deriving anyclass Binary

data ClientMsg where
  StartMsg :: StartFlags -> ClientMsg
  EventMsg :: EventId -> Expr -> ClientMsg
  ResumeMsg :: Word32 -> Expr -> ClientMsg
  deriving stock Generic
  deriving anyclass Binary

newtype StartFlags = StartFlags {unStartFlags :: Expr}
  deriving newtype Binary

newtype ScopeId = ScopeId {unScopeId :: Word32}
  deriving newtype (Binary, Eq)

data RefId = RefId ScopeId Word32
  deriving stock (Generic)
  deriving anyclass (Binary)

newtype EventId = EventId {unEventId :: Word32}
  deriving newtype (Show, Ord, Eq)
  deriving newtype (Binary)

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

type UpdateFn s = forall a. (s -> (s, a)) -> ClickM a

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

data DynVal a where
  ConstVal :: a -> DynVal a
  FromVar :: DynVar a -> DynVal a
  MapVal :: DynVal a -> (a -> b) -> DynVal b
  SplatVal :: DynVal (a -> b) -> DynVal a -> DynVal b
  OverrideSub :: (forall b. SubscribeFn a b -> SubscribeFn a b) -> DynVal a -> DynVal a

type SubscribeFn a b = (a -> b -> ClickM b) -> ClickM ()

instance Functor DynVal where
  fmap = flip MapVal
  {-# INLINE fmap #-}
instance Applicative DynVal where
  pure = ConstVal
  {-# INLINE pure #-}
  (<*>) = SplatVal
  {-# INLINE (<*>) #-}

fromVar :: DynVar a -> DynVal a
fromVar = FromVar
{-# INLINE fromVar #-}

data Subscription a
  = SubscriptionSimple
    { ss_scope :: ScopeId
    , ss_event_id :: Event a
    , ss_callback :: a -> ClickM ()
    }
  | forall b. SubscriptionAccum
    { sa_resource_scope :: ScopeId
    , sa_event_id :: Event a
    , sa_callback :: a -> b -> ClickM b
    , sa_accum_ref :: IORef b
    }

data Finalizer
  = CustomFinalizer
    { cf_resource_scope :: ScopeId
    , cf_callback :: ClickM ()
    }
  | ScopeFinalizer
    { sf_resource_scope :: ScopeId
    , sf_linked_scope :: ScopeId
    }

finalizerScope :: Finalizer -> ScopeId
finalizerScope CustomFinalizer{cf_resource_scope} = cf_resource_scope
finalizerScope ScopeFinalizer{sf_resource_scope} = sf_resource_scope

subscriptionScope :: Subscription a -> ScopeId
subscriptionScope SubscriptionSimple{ss_scope} = ss_scope
subscriptionScope SubscriptionAccum{sa_resource_scope} = sa_resource_scope

-- | JavaScript value, result of evaluating an 'Expr'. Should only
-- contain constructors up to and including 'U8Arr'. I chose to use
-- type alias rather than a data type for easier conversion.
type ValueExpr = Expr

class ToValue a where
  toValue :: a -> ValueExpr
  default toValue :: (Generic a, GToValue (Rep a)) => a -> ValueExpr
  toValue = gToValue . G.from

instance (Generic a, GToValue (Rep a)) => ToValue (Generically a) where
  toValue = gToValue . G.from . (\(Generically x) -> x)

instance ToValue ValueExpr where toValue = Prelude.id

instance ToValue Bool where toValue = Bool . bool 0 1

instance ToValue Int8 where toValue = I8
instance ToValue Int16 where toValue = I16
instance ToValue Int32 where toValue = I32
instance ToValue Int64 where toValue = I64

instance ToValue Word8 where toValue = U8
instance ToValue Word16 where toValue = U16
instance ToValue Word32 where toValue = U32
instance ToValue Word64 where toValue = U64

instance ToValue Float where toValue = F32 . Float32
instance ToValue Double where toValue = F64 . Float64

instance ToValue Int where toValue = I64 . fromIntegral
instance ToValue Word where toValue = U64 . fromIntegral

instance ToValue Char where
  toValue c = Str $ Text.cons c Text.empty

instance ToValue Text where toValue = Str

instance ToValue ByteString where toValue = U8Arr

instance ToValue () where toValue _ = Null

instance ToValue a => ToValue [a] where toValue = Arr . fmap toValue

instance ToValue a => ToValue (Maybe a) where toValue = maybe Null toValue

instance (ToValue a, ToValue b) => ToValue (a, b) where
  toValue (a, b) = toValue [toValue a, toValue b]

instance (ToValue a, ToValue b, ToValue c) => ToValue (a, b, c) where
  toValue (a, b, c) = toValue [toValue a, toValue b, toValue c]
--------------------------------------------------------------------------------

class FromValue a where
  fromValue :: ValueExpr -> Maybe a
  default fromValue :: (Generic a, GFromValue (Rep a)) => ValueExpr -> Maybe a
  fromValue = fmap G.to . gFromValue

instance (Generic a, GFromValue (Rep a)) => FromValue (Generically a) where
  fromValue = fmap (Generically . G.to) . gFromValue

instance FromValue ValueExpr where fromValue = pure

instance FromValue Bool where
  fromValue (Bool 0) = Just False
  fromValue (Bool _) = Just True
  fromValue _ = Nothing

instance FromValue Int8 where
  fromValue (I8 j) = Just j
  fromValue _ = Nothing

instance FromValue Int16 where
  fromValue (I16 j) = Just j
  fromValue _ = Nothing

instance FromValue Int32 where
  fromValue = \case
    I32 j -> Just j
    F64 j -> Just $ floor j.unFloat64
    _ -> Nothing

instance FromValue Int64 where
  fromValue (I64 j) = Just j
  fromValue _ = Nothing

instance FromValue Word8 where
  fromValue (U8 j) = Just j
  fromValue _ = Nothing

instance FromValue Word16 where
  fromValue (U16 j) = Just j
  fromValue _ = Nothing

instance FromValue Word32 where
  fromValue (U32 j) = Just j
  fromValue _ = Nothing

instance FromValue Word64 where
  fromValue (U64 j) = Just j
  fromValue _ = Nothing

instance FromValue Float where
  fromValue (F32 j) = Just j.unFloat32
  fromValue _ = Nothing

instance FromValue Double where
  fromValue = \case
    I32 j -> Just $ fromIntegral j
    F64 (Float64 j) -> Just j
    _ -> Nothing

instance FromValue Int where
  fromValue (I8 j) = Just $ fromIntegral j
  fromValue (I16 j) = Just $ fromIntegral j
  fromValue (I32 j) = Just $ fromIntegral j
  fromValue (I64 j) = Just $ fromIntegral j
  fromValue (U8 j) = Just $ fromIntegral j
  fromValue (U16 j) = Just $ fromIntegral j
  fromValue (U32 j) = Just $ fromIntegral j
  fromValue (U64 j) = Just $ fromIntegral j
  fromValue _ = Nothing

instance FromValue Word where
  fromValue (I8 j) = Just $ fromIntegral j
  fromValue (I16 j) = Just $ fromIntegral j
  fromValue (I32 j) = Just $ fromIntegral j
  fromValue (I64 j) = Just $ fromIntegral j
  fromValue (U8 j) = Just $ fromIntegral j
  fromValue (U16 j) = Just $ fromIntegral j
  fromValue (U32 j) = Just $ fromIntegral j
  fromValue (U64 j) = Just $ fromIntegral j
  fromValue _ = Nothing

instance FromValue Char where
  fromValue = \case
    Str a | Just (c, _) <- Text.uncons a -> Just c
           | otherwise -> Nothing
    _ -> Nothing

instance FromValue Text where
  fromValue = \case Str a -> Just a; _ -> Nothing

instance FromValue ByteString where
  fromValue = \case U8Arr a -> Just a; _ -> Nothing

instance FromValue () where
  fromValue = \case Null -> Just (); _ -> Nothing

instance FromValue a => FromValue [a] where
  fromValue = \case
    Arr xs -> Just (mapMaybe fromValue xs)
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
  gFromValue :: ValueExpr -> Maybe (f a)

instance GFromValue f => GFromValue (M1 m c f) where
  gFromValue = fmap M1 . gFromValue @f

instance GFromValue U1 where
  gFromValue _ = Just U1

instance GFromJSObject (x :*: y) => GFromValue (x :*: y) where
  gFromValue (Obj kvs) = gFromJSObject kvs
  gFromValue _ = Nothing

instance {-# OVERLAPPING #-} FromValue a => GFromValue (S1 s (Rec0 a)) where
  gFromValue = fmap (M1 . K1) . fromValue @a
--------------------------------------------------------------------------------

class GToValue (f :: Type -> Type) where
  gToValue :: f x -> ValueExpr

instance GToValue f => GToValue (M1 m c f) where
  gToValue (M1 f) = gToValue f

instance GToValue U1 where
  gToValue _ = Null

instance GToJSObject (x :*: y) => GToValue (x :*: y) where
  gToValue (x :*: y) = Obj $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToValue a) => GToValue (S1 s (Rec0 a)) where
  gToValue (M1 (K1 a)) = toValue a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, ValueExpr)]

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
  gFromJSObject :: [(Text, ValueExpr)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromValue a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromValue
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
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

data SomeTag = forall a. SomeTag (PromptTag a)

newPromptTag :: forall a. IO (PromptTag a)
newPromptTag = IO \s ->
  let !(# s', t #) = newPromptTag# @a s in (# s', PromptTag t #)
