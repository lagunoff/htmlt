module HtmlT.Types where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Coerce
import Data.String
import GHC.Generics
import HtmlT.Event
import Control.Monad.Fix

import Wasm.Compat.Prim

-- | HtmlT is nothing more than just a newtype over ReaderT HtmlEnv
newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv,
    MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans
  )

data HtmlEnv = HtmlEnv
  { html_current_element :: DOMElement
  -- ^ A DOMElement that will be used as a parent to insert new
  -- content, attributes, properties, listeners etc.
  , html_content_boundary :: Maybe ContentBoundary
  -- ^ Boundary defined by parent scope where new content should be
  -- attached, when Nothing whole parent element is available
  } deriving Generic

type Html = HtmlT RI

-- | A newtype over JSVal which is an instance of Node
-- https://developer.mozilla.org/en-US/docs/Web/API/Node
newtype DOMNode = DOMNode {unDOMNode :: JSVal}

-- | A newtype over JSVal which is an instance of HTMLElement
-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
newtype DOMElement = DOMElement {unDOMElement :: JSVal}

-- | A newtype over JSVal which is an instance of Event
-- https://developer.mozilla.org/en-US/docs/Web/API/Event
newtype DOMEvent = DOMEvent {unDOMEvent :: JSVal}

-- | See https://developer.mozilla.org/en-US/docs/Web/Events for
-- reference, what events are supported by particular elements
newtype EventName = EventName {unEventName :: JSString}

instance IsString EventName where fromString = EventName . toJSString

-- | Two comment nodes that define a boundary and a placeholder to
-- insert additional nodes within the DOM.
data ContentBoundary = ContentBoundary
  { boundary_begin :: DOMNode
  , boundary_end :: DOMNode
  } deriving Generic

-- | Each DOMElement is also a valid DOMNode
nodeFromElement :: DOMElement -> DOMNode
nodeFromElement = coerce

runHtmlT :: HtmlT m a -> HtmlEnv -> m a
runHtmlT h = runReaderT (unHtmlT h)

execHtmlT :: HtmlEnv -> HtmlT m a -> m a
execHtmlT = flip runHtmlT

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty
