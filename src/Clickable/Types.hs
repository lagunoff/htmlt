module Clickable.Types where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Int
import Data.Tuple
import Data.Map (Map)
import GHC.Generics
import GHC.Exts

import Clickable.Protocol

data DynVar a where
  DynVar :: SourceId -> IORef a -> DynVar a
  OverrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
  LensMap :: Lens' s a -> DynVar s -> DynVar a

type UpdateFn s = forall a. (s -> (s, a)) -> ClickM a

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

data DynVal a where
  ConstVal :: a -> DynVal a
  FromVar :: DynVar a -> DynVal a
  MapVal :: DynVal a -> (a -> b) -> DynVal b
  SplatVal :: DynVal (a -> b) -> DynVal a -> DynVal b

instance Functor DynVal where fmap = flip MapVal

instance Applicative DynVal where pure = ConstVal; (<*>) = SplatVal

fromVar :: DynVar a -> DynVal a
fromVar = FromVar

newtype HtmlT m a = HtmlT {unHtmlT :: StateT (Maybe VarId) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

newtype ClickT m a = ClickT {unClickT :: ReaderT InternalEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader InternalEnv, MonadTrans)

type HtmlM = HtmlT ClickM
type ClickM = ClickT IO

instance MonadState InternalState ClickM where
  state f = ClickT $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ swap . f

data InternalEnv = InternalEnv
  { scope :: ResourceScope
  , internal_state_ref :: IORef InternalState
  , send_message :: HaskellMessage -> IO JavaScriptMessage
  } deriving (Generic)

data InternalState = InternalState
  { subscriptions :: [(ResourceScope, SourceId, Any -> ClickM ())]
  , finalizers :: [(ResourceScope, FinalizerVal)]
  , transaction_queue :: Map SourceId (ClickM ())
  , evaluation_queue :: [Expr]
  , next_id :: Int64
  } deriving (Generic)

data FinalizerVal
  = CustomFinalizer (ClickM ())
  | ScopeFinalizer ResourceScope
