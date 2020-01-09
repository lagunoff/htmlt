{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable (Typeable)
import Language.Javascript.JSaddle
import Massaraksh.Dynamic
import Massaraksh.Event
import Pipes as P
import Data.IORef
import qualified Data.Dynamic as D

newtype HtmlT w s t m a = HtmlT { runHtmlT :: ReaderT (HtmlEnv w s t m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (HtmlEnv w s t m), MonadFix)

type HtmlT' w s = HtmlT w s s

type Html w s t m = HtmlT w s t m ()

type Html' w s m = HtmlT w s s m ()

data HtmlEnv w s t m = HtmlEnv
  { hteRootRef       :: RootElmRef
  , hteDynamicRef    :: DynamicRef s t
  , hteSubscriberRef :: SubscriberRef (ComponentT w s t m ()) }

data RootElmRef = RootElmRef
  { relmRead  :: IO JSVal
  , relmWrite :: JSVal -> IO () }

data SubscriberRef a = SubscriberRef
  { sbrefValue         :: Subscriber a
  , sbrefSubscriptions :: IORef [IORef (IO ())] }

data Subscriber a = Subscriber
  { sbscrPrivate :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
  , sbscrPublic  :: Event a -> IO (IO ()) }

type ComponentT w s t m = Producer1 w (HtmlT w s t m)

type Producer1 w = Proxy X () D.Dynamic (Exists w)

data Exists (f :: * -> *) = forall x. Typeable x => Exists { unExists :: f x }

type MonadHtmlBase m = (MonadJSM m, MonadUnliftIO m)

type IsHtml w s t m h = (MonadHtmlBase m, h ~ HtmlT w s t m)

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HtmlT w s t m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (Proxy a' a b' b m) where
  liftJSM' = lift . liftJSM'
#endif

instance MonadHtmlBase m => MonadState s (HtmlT w s s m) where
  get = liftIO =<< asks (dynRead . drefValue . hteDynamicRef)
  put v = liftIO =<< asks (($ const v) . drefModify . hteDynamicRef)

instance MonadUnliftIO m => MonadUnliftIO (HtmlT w s t m) where
  askUnliftIO = HtmlT do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlT)

class Monad m => MonadSplitState s t m | m -> s t where
  sGet :: m s
  sModify :: (s -> t) -> m ()
  sModify f = sGet >>= sPut . f
  sPut :: t -> m ()
  sPut = sModify . const

instance MonadHtmlBase m => MonadSplitState s t (HtmlT w s t m) where
  sGet = liftIO =<< asks (dynRead . drefValue . hteDynamicRef)
  sPut v = liftIO =<< asks (($ const v) . drefModify . hteDynamicRef)

instance MonadTrans (HtmlT w s t) where
  lift = HtmlT . lift

instance (Semigroup a, Applicative m) => Semigroup (HtmlT w s t m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT w s t m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Contravariant Subscriber where
  contramap g Subscriber{..} = Subscriber sbscrPrivate (sbscrPublic . fmap g)

instance Contravariant SubscriberRef where
  contramap g SubscriberRef{..} = SubscriberRef (contramap g sbrefValue) sbrefSubscriptions
