{-# LANGUAGE CPP #-}
module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import GHC.Generics
import Language.Javascript.JSaddle

newtype HtmlT a = HtmlT {unHtmlT :: ReaderT HtmlEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv)
  deriving newtype (MonadFix, MonadCatch, MonadThrow, MonadMask)

data HtmlEnv = HtmlEnv
  { htmlEnv_element :: ElementRef
  , htmlEnv_finalizers :: IORef [IORef (IO ())]
  , htmlEnv_postHooks :: IORef [HtmlT ()]
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

runHtmlT :: HtmlEnv -> HtmlT a -> IO a
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance Semigroup a => Semigroup (HtmlT a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (HtmlT a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

#ifndef ghcjs_HOST_OS
instance MonadJSM HtmlT where
  liftJSM' jsm = HtmlT $ ReaderT (runReaderT (unJSM jsm) . htmlEnv_jsContext)
#endif
