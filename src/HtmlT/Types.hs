module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import GHC.Generics
import GHCJS.Prim
import GHCJS.Types
import GHCJS.Marshal.Pure
import HtmlT.Event

newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans)

data HtmlEnv = HtmlEnv
  { html_current_root :: Node
  , html_insert_before_anchor :: Maybe Node
  , html_reactive_env :: ReactiveEnv
  , html_catch_interactive :: SomeException -> IO ()
  } deriving Generic

type Html = HtmlT IO

newtype Node = Node {unNode :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

runHtmlT :: HtmlEnv -> HtmlT m a -> m a
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Monad m => HasReactiveEnv (HtmlT m) where
  askReactiveEnv = asks html_reactive_env
