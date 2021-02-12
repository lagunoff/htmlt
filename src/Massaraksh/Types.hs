{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Data.String
import Data.Text as T
import GHC.Generics
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Control.Monad.IO.Unlift

type Html = HtmlT IO

newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv,
    MonadFix, MonadCatch, MonadThrow, MonadMask
  )

type MonadHtml m =
  ( MonadReader HtmlEnv m
  , MonadIO m
  , MonadUnliftIO m
  )

data HtmlEnv = HtmlEnv
  { htmlEnv_element :: ElementRef
  , htmlEnv_finalizers :: IORef [IORef (IO ())]
  , htmlEnv_postHooks :: IORef [Html ()]
  , htmlEnv_jsContext :: JSContextRef
  , htmlEnv_catchInteractive :: SomeException -> IO ()
  }
  deriving stock Generic

data ElementRef = ElementRef
  { elementRef_read :: IO Node
  , elementRef_mutate :: (Node -> JSM ()) -> IO ()
  }
  deriving stock Generic

runHtmlT :: HtmlEnv -> HtmlT m x -> m x
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance Semigroup a => Semigroup (Html a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Html a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance (x ~ ()) => IsString (Html x) where
  fromString = text . T.pack where
    text t = do
      elm <- liftIO =<< asks (elementRef_read . htmlEnv_element)
      textNode <- liftJSM (createTextNode t)
      liftJSM (appendChild elm textNode)

instance MonadUnliftIO m => MonadUnliftIO (HtmlT m) where
  withRunInIO act = HtmlT (ReaderT f) where
    f e = withRunInIO \toIO -> act (toIO . runHtmlT e)

#ifndef ghcjs_HOST_OS
instance MonadJSM Html where
  liftJSM' jsm = HtmlT $ ReaderT (runReaderT (unJSM jsm) . htmlEnv_jsContext)
#endif
