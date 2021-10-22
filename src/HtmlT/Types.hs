module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import GHC.Generics
import GHCJS.Prim
import GHCJS.Types
import GHCJS.Marshal.Pure
import HtmlT.Event

-- | HtmlT is nothing more than just a newtype over ReaderT HtmlEnv,
-- that's all!
newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans)

data HtmlEnv = HtmlEnv
  { html_current_root :: DOMNode
  -- ^ A DOMNode that will be used as target to insert new content,
  -- attributes, properties, listeners etc.
  , html_insert_before_anchor :: Maybe DOMNode
  -- ^ When it's @Nothing@ new content will be added to the end of
  -- existing content, when it's @Just anchor@ new content will be
  -- inserted before the @anchor@ element
  , html_reactive_env :: ReactiveEnv
  -- ^ Needed to support running functions from 'HtmlT.Event'
  , html_catch_interactive :: SomeException -> IO ()
  -- ^ Catch haskell exceptions thrown in some some DOM event handler
  -- code
  } deriving Generic

-- | Most applications only will need HtmlT IO, hence this shortcut
type Html = HtmlT IO

-- | A newtype to distinguish between JSVal and DOMNode instances,
-- there is no further division to DOMNode/Element therefore some
-- functions that take DOMNode are unsafe
newtype DOMNode = DOMNode {unDOMNode :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

runHtmlT :: HtmlEnv -> HtmlT m a -> m a
runHtmlT e = flip runReaderT e . unHtmlT
{-# INLINE runHtmlT #-}

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Monad m => HasReactiveEnv (HtmlT m) where
  askReactiveEnv = asks html_reactive_env
