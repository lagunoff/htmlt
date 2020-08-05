{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Data.JSString as JSS
import Data.String
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event

newtype Html a = Html {unHtml :: ReaderT HtmlEnv IO a}
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask )

data HtmlEnv = HtmlEnv
  { he_element           :: ElementRef
  , he_subscribe         :: Subscriber
  , he_post_build        :: IORef [Html ()]
  , he_js_context        :: JSContextRef
  , he_catch_interactive :: SomeException -> IO () }

newtype Subscriber = Subscriber
  {unSubscriber :: forall a. Event a -> Callback a -> Reactive Canceller}

type Subscriptions = IORef [IORef (IO ())]

data ElementRef = ElementRef
  { er_read           :: IO Element
  , er_queue_mutation :: (Element -> JSM ()) -> IO () }

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
  liftJSM' jsm = Html $ ReaderT \HtmlEnv{..} -> runReaderT (unJSM jsm) he_js_context
#endif

instance (x ~ ()) => IsString (Html x) where
  fromString = text . JSS.pack where
    text txt = do
      elm <- liftIO =<< asks (er_read . he_element)
      textNode <- liftJSM (createTextNode txt)
      liftJSM (appendChild elm textNode)
