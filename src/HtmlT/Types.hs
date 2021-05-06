{-# LANGUAGE CPP #-}
module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import GHC.Generics
import GHCJS.Marshal
import GHCJS.Prim
import HtmlT.Event

newtype HtmlT a = HtmlT {unHtmlT :: ReaderT HtmlEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv)
  deriving newtype (MonadFix, MonadCatch, MonadThrow, MonadMask)

data HtmlEnv = HtmlEnv
  { he_current_root :: NodeRef
  , he_finalizers :: Finalizers
  , he_subscriptions :: Subscriptions
  , he_post_hooks :: IORef [HtmlT ()]
  , he_catch_interactive :: SomeException -> IO ()
  }
  deriving stock (Generic)

data NodeRef = NodeRef
  { nr_read :: IO Node
  , nr_mutate :: (Node -> IO ()) -> IO ()
  }
  deriving stock (Generic)

newtype Node = Node {unNode :: JSVal}
  deriving newtype (ToJSVal)

newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}
  deriving newtype (ToJSVal)

runHtmlT :: HtmlEnv -> HtmlT a -> IO a
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance Semigroup a => Semigroup (HtmlT a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (HtmlT a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance MonadSubscribe HtmlT where
  askSubscribe = asks he_subscriptions

instance MonadFinalize HtmlT where
  askFinalizers = asks he_finalizers
