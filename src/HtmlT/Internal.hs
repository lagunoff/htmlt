module HtmlT.Internal where

import Control.Monad.Reader
import GHC.Generics

import HtmlT.Event
import HtmlT.Types
import HtmlT.DOM

-- | Auxiliary type to help implement 'simpleList'
data ElemEnv a = ElemEnv
  { ee_html_env :: HtmlEnv
  , ee_modifier :: Modifier a
  , ee_begin :: DOMNode
  , ee_end :: DOMNode
  } deriving Generic

-- | Insert given node to @html_current_root@ and run action with
-- inserted node as a new root
appendHtmlT :: MonadIO m => DOMNode -> HtmlT m a -> HtmlT m a
appendHtmlT newRootEl html = do
  result <- local (\env -> env
    { html_current_root = newRootEl
    , html_insert_before_anchor = Nothing }) html
  result <$ insertNode newRootEl

-- | Insert new node to @html_current_root@ with respect to
-- @html_insert_before_anchor@`
insertNode :: MonadIO m => DOMNode -> HtmlT m ()
insertNode n = do
  HtmlEnv{..} <- ask
  case html_insert_before_anchor of
    Just anchor -> liftIO $ js_insertBefore html_current_root n anchor
    Nothing -> liftIO $ appendChild html_current_root n

-- | Insert two comment nodes intended to be used as a boundary for
-- dynamic content and as arguments to @removeBetween@ to clear the
-- content in the finalizer
insertBoundaries :: MonadIO m => HtmlT m (DOMNode, DOMNode)
insertBoundaries = do
  beginAnchor <- liftIO $ createComment ">>> begin"
  endAnchor <- liftIO $ createComment "<<< end"
  insertNode beginAnchor
  insertNode endAnchor
  return (beginAnchor, endAnchor)
