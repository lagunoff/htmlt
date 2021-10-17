module HtmlT.Internal where

import Control.Monad.Reader
import GHC.Generics

import HtmlT.Event
import HtmlT.Types
import HtmlT.DOM

data ElemEnv a = ElemEnv
  { ee_html_env :: HtmlEnv
  , ee_ref :: DynRef a
  , ee_modifier :: Modifier a
  , ee_begin :: Node
  , ee_end :: Node
  } deriving Generic

appendHtmlT :: MonadIO m => Node -> HtmlT m a -> HtmlT m a
appendHtmlT newRootEl html = do
  result <- local (\env -> env
    { html_current_root = newRootEl
    , html_insert_before_anchor = Nothing }) html
  result <$ insertNode newRootEl

insertNode :: MonadIO m => Node -> HtmlT m ()
insertNode n = do
  HtmlEnv{..} <- ask
  case html_insert_before_anchor of
    Just anchor -> liftIO $ js_insertBefore html_current_root n anchor
    Nothing -> liftIO $ appendChild html_current_root n

insertBoundaries :: MonadIO m => HtmlT m (Node, Node)
insertBoundaries = do
  beginAnchor <- liftIO $ createComment ">>> BOUNDARY BEGIN"
  endAnchor <- liftIO $ createComment "<<< BOUNDARY END"
  insertNode beginAnchor
  insertNode endAnchor
  return (beginAnchor, endAnchor)
