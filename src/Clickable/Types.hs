module Clickable.Types where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.State
import Data.Binary
import Data.IORef
import Data.Tuple
import Data.Map (Map)
import GHC.Generics
import GHC.Exts

import Clickable.Protocol
import Clickable.Protocol.Value (Int32Le, Value)

newtype EventId a = EventId {unEventId :: Int32Le}
  deriving newtype (Show, Num, Ord, Eq, Binary)

data DynVar a where
  SourceVar :: EventId a -> IORef a -> DynVar a
  OverrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
  LensMap :: Lens' s a -> DynVar s -> DynVar a

type UpdateFn s = forall a. (s -> (s, a)) -> ClickM a

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

data DynVal a where
  ConstVal :: a -> DynVal a
  FromVar :: DynVar a -> DynVal a
  MapVal :: DynVal a -> (a -> b) -> DynVal b
  MapHoldVal :: DynVal a -> (a -> b) -> EventId b -> IORef b -> DynVal b
  -- ^ todo: need redesign
  SplatVal :: DynVal (a -> b) -> DynVal a -> DynVal b
  OverrideSub :: (forall b. SubscribeFn a b -> SubscribeFn a b) -> DynVal a -> DynVal a

type SubscribeFn a b = (a -> b -> ClickM b) -> ClickM ()

instance Functor DynVal where fmap = flip MapVal

instance Applicative DynVal where pure = ConstVal; (<*>) = SplatVal

fromVar :: DynVar a -> DynVal a
fromVar = FromVar

newtype HtmlT m a = HtmlT {unHtmlT :: StateT (Maybe VarId) m a}
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans,
    MonadCatch, MonadMask, MonadThrow
  )

newtype ClickT m a = ClickT {unClickT :: ReaderT InternalEnv m a}
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader InternalEnv,
    MonadTrans, MonadCatch, MonadMask, MonadThrow
  )

type HtmlM = HtmlT ClickM
type ClickM = ClickT IO

instance MonadState InternalState ClickM where
  state f = ClickT $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ swap . f

data InternalEnv = InternalEnv
  { scope :: ResourceScope
  , internal_state_ref :: IORef InternalState
  , eval_expr :: Expr -> IO Value
  } deriving (Generic)

data InternalState = InternalState
  { subscriptions :: [Subscription Any]
  , finalizers :: [Finalizer]
  , transaction_queue :: Map AnyEventId (ClickM ())
  , evaluation_queue :: [Expr]
  , next_id :: Int32Le
  } deriving (Generic)

data Subscription a
  = SubscriptionSimple
    { ss_scope :: ResourceScope
    , ss_event_id :: EventId a
    , ss_callback :: a -> ClickM ()
    }
  | forall b. SubscriptionAccum
    { sa_resource_scope :: ResourceScope
    , sa_event_id :: EventId a
    , sa_callback :: a -> b -> ClickM b
    , sa_accum_ref :: IORef b
    }

data Finalizer
  = CustomFinalizer
    { cf_resource_scope :: ResourceScope
    , cf_callback :: ClickM ()
    }
  | ScopeFinalizer
    { sf_resource_scope :: ResourceScope
    , sf_linked_scope :: ResourceScope
    }

finalizerScope :: Finalizer -> ResourceScope
finalizerScope CustomFinalizer{cf_resource_scope} = cf_resource_scope
finalizerScope ScopeFinalizer{sf_resource_scope} = sf_resource_scope

subscriptionScope :: Subscription a -> ResourceScope
subscriptionScope SubscriptionSimple{ss_scope} = ss_scope
subscriptionScope SubscriptionAccum{sa_resource_scope} = sa_resource_scope
