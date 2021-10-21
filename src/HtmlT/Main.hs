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

data RunningApp = RunningApp
  { runapp_html_env :: HtmlEnv
  , runapp_boundary_begin :: Node
  , runapp_boundary_end :: Node
  } deriving Generic

startWithOptions :: StartOpts -> Html a -> IO (a, RunningApp)
startWithOptions StartOpts{..} render = mdo
  postHooks <- newIORef []
  begin <- createComment ">>> begin"
  end <- createComment "<<< end"
  appendChild startopts_root_element begin
  appendChild startopts_root_element end
  let
    htmlEnv = HtmlEnv
      { html_current_root = startopts_root_element
      , html_insert_before_anchor = Just end
      , html_reactive_env = startopts_reactive_env
      , html_post_hooks = postHooks
      , html_catch_interactive = throwM
      }
    runApp = RunningApp htmlEnv begin end
  result <- runHtmlT htmlEnv render
  liftIO (readIORef postHooks >>= sequence_)
  onBeforeUnload do
    fins <- readIORef $ renv_finalizers startopts_reactive_env
    sequence_ fins
  pure (result, runApp)

attachTo :: Node -> Html a -> IO (a, RunningApp)
attachTo rootEl render = do
  fins <- liftIO $ newIORef []
  subs <- liftIO $ H.new
  startWithOptions (StartOpts (ReactiveEnv subs fins) rootEl) render

attachToBody :: Html a -> IO (a, RunningApp)
attachToBody h = getCurrentBody >>= (`attachTo` h)

detach :: RunningApp -> IO ()
detach RunningApp{..} = do
  finalizers <- readIORef . renv_finalizers . html_reactive_env $
    runapp_html_env
  sequence_ finalizers
  removeBetween (html_current_root runapp_html_env) runapp_boundary_begin
    runapp_boundary_end
  removeChild (html_current_root runapp_html_env) runapp_boundary_begin
  removeChild (html_current_root runapp_html_env) runapp_boundary_end
