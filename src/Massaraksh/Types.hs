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

newtype Html a = Html {unHtml :: ReaderT HtmlEnv IO a}
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv,
    MonadFix, MonadCatch, MonadThrow, MonadMask, MonadUnliftIO
  )

type MonadHtml m = (MonadReader HtmlEnv m, MonadIO m, MonadUnliftIO m)

data HtmlEnv = HtmlEnv
  { htmlEnv_element :: ElementRef
  , htmlEnv_finalizers :: IORef [IORef (IO ())]
  , htmlEnv_postHooks :: IORef [Html ()]
  , htmlEnv_jsContext :: JSContextRef
  , htmlEnv_catchInteractive :: SomeException -> IO ()
  }
  deriving stock (Generic)

data ElementRef = ElementRef
  { elementRef_read :: IO Node
  , elementRef_mutate :: (Node -> JSM ()) -> IO ()
  }
  deriving stock (Generic)

runHtml :: HtmlEnv -> Html x -> IO x
runHtml e = flip runReaderT e . unHtml
{-# INLINE runHtml #-}

instance Semigroup a => Semigroup (Html a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Html a) where
  mempty = Html $ ReaderT \_ -> pure mempty

instance (x ~ ()) => IsString (Html x) where
  fromString = text . T.pack where
    text t = do
      elm <- liftIO =<< asks (elementRef_read . htmlEnv_element)
      textNode <- liftJSM (createTextNode t)
      liftJSM (appendChild elm textNode)

#ifndef ghcjs_HOST_OS
instance MonadJSM Html where
  liftJSM' jsm = Html $ ReaderT (runReaderT (unJSM jsm) . htmlEnv_jsContext)
#endif
