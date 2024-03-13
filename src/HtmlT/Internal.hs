module HtmlT.Internal where

import Control.Monad.Reader
import GHC.Generics
import Wasm.Compat.Prim

import HtmlT.Event
import HtmlT.Types
import HtmlT.DOM

-- | Auxiliary type helps to implement 'simpleList'
data ElemEnv a = ElemEnv
  { ee_html_env :: HtmlEnv
  , ee_dyn_ref :: DynRef a
  } deriving Generic

-- | Insert given node to @html_current_element@ and run action with
-- inserted node as a new root
appendHtmlT :: MonadIO m => DOMElement -> HtmlT m a -> HtmlT m a
appendHtmlT newRootEl html = do
  result <- local (\env -> env
    { html_current_element = newRootEl
    , html_content_boundary = Nothing }) html
  result <$ insertNode (nodeFromElement newRootEl)

-- | Insert new node to the end of current boundary
insertNode :: MonadIO m => DOMNode -> HtmlT m ()
insertNode n = do
  rootEl <- asks html_current_element
  boundary <- asks html_content_boundary
  case boundary of
    Just b -> liftIO $
      js_insertBefore rootEl n b.boundary_end
    Nothing -> liftIO $ appendChild rootEl n

-- | Insert two DOM Comment nodes intended to be used as a boundary for
-- dynamic content.
insertBoundary :: MonadIO m => HtmlT m ContentBoundary
insertBoundary = do
  boundary_begin <- liftIO $ createComment $ toJSString "ContentBoundary {{"
  boundary_end <- liftIO $ createComment $ toJSString "}}"
  insertNode boundary_begin
  insertNode boundary_end
  return ContentBoundary {boundary_begin, boundary_end}
