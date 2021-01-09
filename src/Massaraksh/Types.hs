{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event

newtype Html a = Html {unHtml :: ReaderT HtmlEnv IO a}
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask )

data HtmlEnv = HtmlEnv
  { htnvRootRef    :: RootRef
  , htnvSubscribe  :: Subscriber
  , htnvPostBuild  :: IORef [Html ()]
  , htnvJsContext  :: JSContextRef
  , htnvCatchInteractive :: SomeException -> IO () }

data RootRef = RootRef
  { rrfRoot   :: Node
  , rrfMutate :: (Node -> JSM ()) -> IO ()
  , rrfAdopt  :: IO (Maybe Node) }

newtype Subscriber = Subscriber
  {unSubscriber :: forall a. Event a -> Callback a -> Reactive Canceller}

type Subscriptions = IORef [IORef (IO ())]

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
  liftJSM' jsm = Html $ ReaderT (runReaderT (unJSM jsm) . htnvJsContext)
#endif
