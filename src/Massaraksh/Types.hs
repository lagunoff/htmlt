{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State
import Control.Natural
import Data.IORef
import Language.Javascript.JSaddle
import Massaraksh.Dynamic
import Massaraksh.Event

newtype HtmlT s t m a = HtmlT { runHtmlT :: ReaderT (HtmlEnv s t m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (HtmlEnv s t m), MonadFix)

type HtmlT' w s = HtmlT s s

type Html w s t = HtmlT s t JSM

type Html' w s = Html w s s

type HtmlEvalT w s t m = forall x. w x -> HtmlT s t m x

type HtmlRecT w s t m = HtmlEvalT w s t m -> HtmlEvalT w s t m

type HtmlRecT' w s m = HtmlRecT w s s m

data HtmlEnv s t m = HtmlEnv
  { hteElement    :: ElementRef
  , hteModel      :: DynamicRef s t
  , hteSubscriber :: SubscriberRef (Exist (HtmlT s t m)) }

data ElementRef = ElementRef
  { relmRead  :: IO Element
  , relmWrite :: Element -> IO () }

data SubscriberRef a = SubscriberRef
  { sbrefValue         :: Subscriber a
  , sbrefSubscriptions :: IORef [IORef (IO ())] }

data Subscriber a = Subscriber
  { sbscrPrivate :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
  , sbscrPublic  :: Event a -> IO (IO ()) }

data Exist (f :: * -> *) = forall x. Exist (f x)

type HtmlBase m = (MonadJSM m, MonadUnliftIO m)

type Node = JSVal

type Element = JSVal

htmlFix :: (HtmlEvalT w s t m -> HtmlEvalT w s t m) -> HtmlEvalT w s t m
htmlFix f = f (htmlFix f)

instance HtmlBase m => MonadState s (HtmlT s s m) where
  get = liftIO =<< asks (dynRead . drefValue . hteModel)
  put v = liftIO =<< asks (($ const v) . drefModify . hteModel)

instance MonadUnliftIO m => MonadUnliftIO (HtmlT s t m) where
  askUnliftIO = HtmlT do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlT)

class Monad m => OpticalState s t m | m -> s t where
  oget :: m s
  omodify :: (s -> t) -> m ()
  omodify f = oget >>= oput . f
  oput :: t -> m ()
  oput = omodify . const

instance HtmlBase m => OpticalState s t (HtmlT s t m) where
  oget = liftIO =<< asks (dynRead . drefValue . hteModel)
  oput v = liftIO =<< asks (($ const v) . drefModify . hteModel)

instance MonadTrans (HtmlT s t) where
  lift = HtmlT . lift

instance (Semigroup a, Applicative m) => Semigroup (HtmlT s t m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT s t m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Contravariant Subscriber where
  contramap g Subscriber{..} = Subscriber sbscrPrivate (sbscrPublic . fmap g)

instance Contravariant SubscriberRef where
  contramap g SubscriberRef{..} = SubscriberRef (contramap g sbrefValue) sbrefSubscriptions

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HtmlT s t m) where
  liftJSM' = lift . liftJSM'
#endif
