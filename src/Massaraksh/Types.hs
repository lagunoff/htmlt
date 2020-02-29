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

newtype HtmlT s m a = HtmlT { runHtmlT :: ReaderT (HtmlEnv s m) m a }
  deriving (
    Functor, Applicative, Monad, MonadIO, MonadReader (HtmlEnv s m), MonadFix
  )

type HtmlM s = HtmlT s JSM

type Html s = HtmlM s ()

type HtmlEval w s m = w ~> HtmlT s m

type HtmlRec w s m = (w ~> HtmlT s m) -> w ~> HtmlT s m

type HtmlLift s t a b m = HtmlT s m ~> HtmlT a m

type HtmlInterleave s a m x = (HtmlT s m ~> HtmlT a m) -> HtmlT a m x

data HtmlEnv s m = HtmlEnv
  { hteElement    :: ElementRef
  , hteModel      :: DynamicRef s s
  , hteSubscriber :: SubscriberRef (Exist (HtmlT s m)) }

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

type HtmlBase m = (MonadJSM m, MonadUnliftIO m, MonadFix m)

type Node = JSVal

type Element = JSVal

instance HtmlBase m => MonadState s (HtmlT s m) where
  get = liftIO =<< asks (dynRead . drefValue . hteModel)
  put v = liftIO =<< asks (($ const v) . drefModify . hteModel)

instance MonadUnliftIO m => MonadUnliftIO (HtmlT s m) where
  askUnliftIO = HtmlT do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlT)

instance MonadTrans (HtmlT s) where
  lift = HtmlT . lift

instance (Semigroup a, Applicative m) => Semigroup (HtmlT s m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT s m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Contravariant Subscriber where
  contramap g Subscriber{..} =
    Subscriber sbscrPrivate (sbscrPublic . fmap g)

instance Contravariant SubscriberRef where
  contramap g SubscriberRef{..} =
    SubscriberRef (contramap g sbrefValue) sbrefSubscriptions

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HtmlT s m) where
  liftJSM' = lift . liftJSM'
#endif
