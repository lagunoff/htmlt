-- | Start and stop browser application
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
  { startopts_reactive_env :: ReactiveEnv
  , startopts_root_element :: Node
  } deriving Generic

startWithOptions :: StartOpts -> Html a -> IO (a, HtmlEnv)
startWithOptions StartOpts{..} render = do
  postHooks <- liftIO (newIORef [])
  let
    htmlEnv = HtmlEnv
      { html_current_root = startopts_root_element
      , html_insert_before_anchor = Nothing
      , html_reactive_env = startopts_reactive_env
      , html_post_hooks = postHooks
      , html_catch_interactive = throwM
      }
  result <- runHtmlT htmlEnv render
  liftIO (readIORef postHooks >>= sequence_)
  onBeforeUnload do
    fins <- readIORef $ renv_finalizers startopts_reactive_env
    sequence_ fins
  pure (result, htmlEnv)

attachTo :: Node -> Html a -> IO (a, HtmlEnv)
attachTo rootEl render = do
  fins <- liftIO $ newIORef []
  subs <- liftIO $ H.new
  startWithOptions (StartOpts (ReactiveEnv subs fins) rootEl) render

attachToBody :: Html a -> IO (a, HtmlEnv)
attachToBody h = getCurrentBody >>= (`attachTo` h)

detach :: HtmlEnv -> IO ()
detach HtmlEnv{..} = do
  fins <- readIORef $ renv_finalizers html_reactive_env
  sequence_ fins
  removeAllChilds html_current_root
