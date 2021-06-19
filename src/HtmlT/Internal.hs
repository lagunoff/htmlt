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
  } deriving Generic

appendHtmlT :: MonadIO m => Node -> HtmlT m a -> HtmlT m a
appendHtmlT newRootEl html = do
  rootEl <- asks he_current_root
  result <- local (\env -> env { he_current_root = newRootEl }) html
  result <$ liftIO (appendChild rootEl newRootEl)
