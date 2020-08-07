{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Data.Text as T
import Data.String
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event

newtype Html a = Html {unHtml :: ReaderT HtmlEnv IO a}
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask )

data HtmlEnv = HtmlEnv
  { htnvElement   :: ElementRef
  , htnvSubscribe :: Subscriber
  , htnvPostBuild :: IORef [Html ()]
  , htnvJsContext :: JSContextRef
  , htnvCatchInteractive :: SomeException -> IO () }

newtype Subscriber = Subscriber
  {unSubscriber :: forall a. Event a -> Callback a -> Reactive Canceller}

type Subscriptions = IORef [IORef (IO ())]

data ElementRef = ElementRef
  { elrfRead          :: IO Node
  , elrfQueueMutation :: (Node -> JSM ()) -> IO () }

data Exist (f :: * -> *) = forall x. Exist (f x)

runHtml :: HtmlEnv -> Html x -> IO x
runHtml e = flip runReaderT e . unHtml
{-# INLINE runHtml #-}

instance Semigroup a => Semigroup (Html a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Html a) where
  mempty = Html $ ReaderT \_ -> pure mempty

#ifndef ghcjs_HOST_OS
instance MonadJSM Html where
  liftJSM' jsm = Html $ ReaderT \HtmlEnv{..} -> runReaderT (unJSM jsm) htnvJsContext
#endif

instance (x ~ ()) => IsString (Html x) where
  fromString = text . T.pack where
    text txt = do
      elm <- liftIO =<< asks (elrfRead . htnvElement)
      textNode <- liftJSM (createTextNode txt)
      liftJSM (appendChild elm textNode)
