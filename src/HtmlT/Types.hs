module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import GHC.Generics
import GHCJS.Prim
import GHCJS.Types
import HtmlT.Event

newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans)

data HtmlEnv = HtmlEnv
  { html_current_root :: Node
  , html_finalizers :: Finalizers
  , html_subscriptions :: Subscriptions
  , html_post_hooks :: IORef [IO ()]
  , html_catch_interactive :: SomeException -> IO ()
  } deriving Generic

type Html = HtmlT IO

newtype Node = Node {unNode :: JSVal}
  deriving anyclass (IsJSVal)

newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}
  deriving anyclass (IsJSVal)

runHtmlT :: HtmlEnv -> HtmlT m a -> m a
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Monad m => MonadSubscribe (HtmlT m) where
  askSubscribe = asks html_subscriptions

instance Monad m => MonadFinalize (HtmlT m) where
  askFinalizers = asks html_finalizers
