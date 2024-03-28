module Clickable.Types where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List qualified as List
import Data.Text (Text)
import Data.Tuple
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import GHC.Exts hiding (build)
import Unsafe.Coerce

import Clickable.FFI
import Wasm.Compat.Prim

data DynVar a where
  DynVar :: SourceId -> IORef a -> DynVar a

data DynVal a where
  ConstVal :: a -> DynVal a
  FromVar :: DynVar a -> DynVal a
  MapVal :: DynVal a -> (a -> b) -> DynVal b
  SplatVal :: DynVal (a -> b) -> DynVal a -> DynVal b

instance Functor DynVal where fmap = flip MapVal

instance Applicative DynVal where pure = ConstVal; (<*>) = SplatVal

fromVar :: DynVar a -> DynVal a
fromVar = FromVar

newtype ResourceScope = ResourceScope {unResourceScope :: Int}
  deriving newtype (Eq, Ord, Show)

newtype SourceId = SourceId {unSourceId :: Int}
  deriving newtype (Eq, Ord, Show)

newtype HtmlM a = HtmlM { unHtmlM :: ReaderT JSVal ClickM a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader JSVal)

newtype ClickM a = ClickM {unClickM :: ReaderT InternalEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader InternalEnv)

instance MonadState InternalState ClickM where
  state f = ClickM $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ swap . f

data InternalEnv = InternalEnv
  { scope :: ResourceScope
  , internal_state_ref :: IORef InternalState
  } deriving (Generic)

data InternalState = InternalState
  { subscriptions :: [(ResourceScope, SourceId, Any -> ClickM ())]
  , finalizers :: [(ResourceScope, FinalizerVal)]
  , transaction_queue :: Map SourceId (ClickM ())
  , next_id :: Int
  } deriving (Generic)

data FinalizerVal
  = CustomFinalizer (ClickM ())
  | ScopeFinalizer ResourceScope
