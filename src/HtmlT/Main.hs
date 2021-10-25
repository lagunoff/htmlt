-- | Start and stop browser application
module HtmlT.Main where

import Control.Monad.Catch
import Data.IORef
import GHC.Generics

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Types

data StartOpts = StartOpts
  { startopts_reactive_env :: ReactiveEnv
  , startopts_root_element :: DOMElement
  } deriving Generic

data RunningApp = RunningApp
  { runapp_html_env :: HtmlEnv
  , runapp_boundary_begin :: DOMNode
  , runapp_boundary_end :: DOMNode
  } deriving Generic

startWithOptions :: StartOpts -> Html a -> IO (a, RunningApp)
startWithOptions StartOpts{..} render = mdo
  begin <- createComment "dynamic content {{"
  end <- createComment "}}"
  appendChild startopts_root_element begin
  appendChild startopts_root_element end
  let
    htmlEnv = HtmlEnv
      { html_current_root = startopts_root_element
      , html_insert_before_anchor = Just end
      , html_reactive_env = startopts_reactive_env
      , html_catch_interactive = throwM
      }
    runApp = RunningApp htmlEnv begin end
  result <- runHtmlT htmlEnv render
  onBeforeUnload do
    fins <- readIORef $ renv_finalizers startopts_reactive_env
    sequence_ fins
  pure (result, runApp)

attachTo :: DOMElement -> Html a -> IO (a, RunningApp)
attachTo rootEl render = do
  renv <- newReactiveEnv
  startWithOptions (StartOpts renv rootEl) render

attachToBody :: Html a -> IO (a, RunningApp)
attachToBody h = getCurrentBody >>= (`attachTo` h)

detach :: RunningApp -> IO ()
detach RunningApp{..} = do
  finalizers <- readIORef . renv_finalizers . html_reactive_env $
    runapp_html_env
  sequence_ finalizers
  unsafeRemoveBetween (html_current_root runapp_html_env) runapp_boundary_begin
    runapp_boundary_end
  removeChild (html_current_root runapp_html_env) runapp_boundary_begin
  removeChild (html_current_root runapp_html_env) runapp_boundary_end
