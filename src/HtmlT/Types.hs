module HtmlT.Types where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Coerce
import Data.Generics.Product
import Data.Text
import GHC.Generics
import GHCJS.Marshal.Pure
import GHCJS.Prim
import GHCJS.Types
import HtmlT.Event
import Control.Monad.Trans.Control
import Control.Monad.Base


-- | HtmlT is nothing more than just a newtype over ReaderT HtmlEnv
newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HtmlEnv
    , MonadFix, MonadCatch, MonadThrow, MonadMask, MonadTrans, MonadTransControl)

data HtmlEnv = HtmlEnv
  { html_current_element :: CurrentDOMElement
  -- ^ A DOMElement that will be used as a parent to insert new
  -- content, attributes, properties, listeners etc.
  , html_content_boundary :: MaybeContentBoundary
  -- ^ Boundary defined by parent scope where new content should be
  -- attached, when Nothing whole parent element is available
  , html_reactive_env :: ReactiveEnv
  -- ^ Needed to implement 'HasReactiveEnv'
  } deriving Generic

newtype MaybeContentBoundary = MaybeContentBoundary
  { unMaybeContentBoundary :: Maybe ContentBoundary
  }

newtype CurrentDOMElement = CurrentDOMElement
  { unCurrentDOMElement :: DOMElement
  }

-- | Most applications will only need HtmlT IO, hence this shortcut
type Html = HtmlT IO

type MonadHtml e m =
  ( MonadIO m
  , MonadReader e m
  , HasType CurrentDOMElement e
  , HasType MaybeContentBoundary e
  )

type MonadHtmlDyn e m =
  ( MonadHtml e m
  , MonadBaseControl IO m
  , HasType ReactiveEnv e
  )

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

-- | Two 'DOMNode's (comments) that define boundaries for another
-- DOM content
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

deriving newtype instance MonadBase b m => MonadBase b (HtmlT m)
deriving newtype instance (MonadBase b (HtmlT m), MonadBaseControl b m) => MonadBaseControl b (HtmlT m)
