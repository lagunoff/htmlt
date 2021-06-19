module HtmlT.Main where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import GHC.Generics
import HtmlT.DOM
import HtmlT.Event
import HtmlT.Types
import qualified HtmlT.HashMap as H

data StartOpts = StartOpts
  { startopts_finalizers :: Finalizers
  , startopts_subscriptions :: Subscriptions
  , startopts_root_element :: Node
  } deriving Generic

attachOpts :: StartOpts -> HtmlT IO a -> IO (a, HtmlEnv)
attachOpts StartOpts{..} render = do
  postHooks <- liftIO (newIORef [])
  let
    htmlEnv = HtmlEnv
      { he_current_root = startopts_root_element
      , he_finalizers = startopts_finalizers
      , he_subscriptions = startopts_subscriptions
      , he_post_hooks = postHooks
      , he_catch_interactive = throwM
      }
  result <- runHtmlT htmlEnv render
  liftIO (readIORef postHooks >>= sequence_)
  onBeforeUnload do
    fins <- readIORef (unFinalizers startopts_finalizers)
    sequence_ fins
  pure (result, htmlEnv)

attach :: Node -> HtmlT IO a -> IO (a, HtmlEnv)
attach rootEl render = do
  fins <- liftIO $ Finalizers <$> newIORef []
  subs <- liftIO $ Subscriptions <$> H.new
  attachOpts (StartOpts fins subs rootEl) render

attachToBody :: HtmlT IO a -> IO (a, HtmlEnv)
attachToBody h = getBody >>= (`attach` h)

detach :: HtmlEnv -> IO ()
detach HtmlEnv{..} = do
  fins <- readIORef (unFinalizers he_finalizers)
  sequence_ fins
  removeAllChilds he_current_root
