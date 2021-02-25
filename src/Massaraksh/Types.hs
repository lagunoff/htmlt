{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import GHC.Generics
import Language.Javascript.JSaddle
import Control.Monad.IO.Unlift

newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv)
  deriving newtype (MonadFix, MonadCatch, MonadThrow, MonadMask)

type Html = HtmlT IO

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

newtype Node = Node {unNode :: JSVal}
  deriving newtype (MakeArgs, MakeObject, ToJSVal)

runHtmlT :: HtmlEnv -> HtmlT m a -> m a
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance Semigroup a => Semigroup (Html a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Html a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

#ifndef ghcjs_HOST_OS
instance MonadJSM Html where
  liftJSM' jsm = HtmlT $ ReaderT (runReaderT (unJSM jsm) . htmlEnv_jsContext)
#endif

instance MonadUnliftIO m => MonadUnliftIO (HtmlT m) where
  withRunInIO inner = HtmlT $ withRunInIO \run -> inner $ run . unHtmlT
