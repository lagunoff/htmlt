module HtmlT.Internal where

import Control.Monad.Reader
import GHC.Generics
import Data.Generics.Product

import HtmlT.Event
import HtmlT.Types
import HtmlT.DOM

-- | Auxiliary type to help implement 'simpleList'
data ElemEnv e a = ElemEnv
  { ee_html_env :: e
  , ee_modifier :: Modifier a
  , ee_boundary :: ContentBoundary
  } deriving Generic

-- | Insert given node to @html_current_element@ and run action with
-- inserted node as a new root
appendHtml :: MonadHtml e m => DOMElement -> m a -> m a
appendHtml newRootEl html = do
  result <- local
    ( setTyped (CurrentDOMElement newRootEl)
    . setTyped (MaybeContentBoundary Nothing)
    ) html
  result <$ insertNode (nodeFromElement newRootEl)

-- | Insert new node to the end of current boundary
insertNode :: MonadHtml e m => DOMNode -> m ()
insertNode n = do
  contBoundary <- asks (unMaybeContentBoundary . getTyped)
  curElm <- asks (unCurrentDOMElement . getTyped)
  case contBoundary of
    Just ContentBoundary{..} -> liftIO $
      js_insertBefore curElm n boundary_end
    Nothing -> liftIO $ appendChild curElm n

-- | Insert two DOM Comment nodes intended to be used as a boundary for
-- dynamic content.
insertBoundary :: MonadHtml e m => m ContentBoundary
insertBoundary = do
  boundary_begin <- liftIO $ createComment "ContentBoundary {{"
  boundary_end <- liftIO $ createComment "}}"
  insertNode boundary_begin
  insertNode boundary_end
  return ContentBoundary{..}
