module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Coerce
import Data.Text
import GHC.Generics
import GHCJS.Marshal.Pure
import GHCJS.Prim
import GHCJS.Types
import HtmlT.Event

-- | HtmlT is nothing more than just a newtype over ReaderT HtmlEnv
newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans)

data HtmlEnv = HtmlEnv
  { html_current_element :: DOMElement
  -- ^ A DOMElement that will be used as a parent to insert new
  -- content, attributes, properties, listeners etc.
  , html_insert_before_anchor :: Maybe DOMNode
  -- ^ When this field is @Nothing@ new content will be added to the
  -- end of existing content, when it's @Just anchor@ new content will
  -- be inserted before the @anchor@ node
  , html_reactive_env :: ReactiveEnv
  -- ^ Needed to implement 'HasReactiveEnv'
  , html_catch_interactive :: SomeException -> IO ()
  -- ^ Catch haskell exceptions thrown from a DOM event handler
  } deriving Generic

-- | Most applications will only need HtmlT IO, hence this shortcut
type Html = HtmlT IO

-- | A newtype over JSVal which is an instance of Node
-- https://developer.mozilla.org/en-US/docs/Web/API/Node
newtype DOMNode = DOMNode {unDOMNode :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

-- | A newtype over JSVal which is an instance of HTMLElement
-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
newtype DOMElement = DOMElement {unDOMElement :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

-- | A newtype over JSVal which is an instance of Event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}
  deriving anyclass (IsJSVal)
  deriving newtype (PToJSVal, PFromJSVal)

-- | Untyped for simplicity and because it was annoying to not find
-- some of the new Events in ghcjs-dom where these names are
-- representated by data constructors
type EventName = Text

-- | Each DOMElement is also a valid DOMNode
nodeFromElement :: DOMElement -> DOMNode
nodeFromElement = coerce

runHtmlT :: HtmlEnv -> HtmlT m a -> m a
runHtmlT e = flip runReaderT e . unHtmlT

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Monad m => HasReactiveEnv (HtmlT m) where
  askReactiveEnv = asks html_reactive_env
