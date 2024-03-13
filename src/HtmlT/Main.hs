-- | Start and stop browser application
module HtmlT.Main where

import Control.Monad
import Data.IORef
import GHC.Generics
import Wasm.Compat.Prim

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Types

data StartOpts = StartOpts
  { startopts_reactive_env :: ReactiveEnv
  -- ^ Typically the program should only have one instance of
  -- 'ReactiveEnv', so when there are multiple running 'HtmlT'
  -- applications, use this field to share existing 'ReactiveEnv'
  -- between all of them
  , startopts_root_element :: DOMElement
  -- ^ HTMLElement where to attach the elements created by the
  -- application
  , startopts_wait_document_load :: Bool
  -- ^ If True block IO action until main document is fully loaded
  , startopts_unload_call_finalizers :: Bool
  -- ^ If True run finalizers on @beforeunload@ event. This happens
  -- just before browser tab is closed, the code in finalizers should
  -- only consist of non-blocking IO
  } deriving Generic

-- | Needed to manually finalize and detach the application
data RunningApp = RunningApp
  { runapp_html_env :: HtmlEnv
  , runapp_boundary :: ContentBoundary
  } deriving Generic

-- | Start 'HtmlT' application applying customizations described by
-- StartOpts argument
attachOptions :: StartOpts -> Html a -> IO (a, RunningApp)
attachOptions StartOpts{..} render = mdo
  -- TODO: doesn't work with javascript-backend
  -- when startopts_wait_document_load
  --   js_waitDocumentLoad
  begin <- createComment "ContentBoundary {{"
  end <- createComment "}}"
  appendChild startopts_root_element begin
  appendChild startopts_root_element end
  let
    boundary = ContentBoundary begin end
    htmlEnv = HtmlEnv
      { html_current_element = startopts_root_element
      , html_content_boundary = Just boundary
      , html_reactive_env = startopts_reactive_env
      }
    runApp = RunningApp htmlEnv boundary
  result <- execHtmlT htmlEnv render
  when startopts_unload_call_finalizers $ onBeforeUnload $ do
    finalizers <- readIORef $ renv_finalizers startopts_reactive_env
    applyFinalizer startopts_reactive_env finalizers
  return (result, runApp)

-- | Start the application and attach it to the given HTMLElement
attachTo :: DOMElement -> Html a -> IO (a, RunningApp)
attachTo rootEl html = do
  renv <- newReactiveEnv
  attachOptions (StartOpts renv rootEl True True) html

-- | Start the application and attach it to current <body> element
attachToBody :: Html a -> IO (a, RunningApp)
attachToBody html = do
  bodyEl <- getCurrentBody
  attachTo bodyEl html

-- | Run finalizers and detach created elements from the DOM
detach :: RunningApp -> IO ()
detach app = do
  finalizers <- readIORef app.runapp_html_env.html_reactive_env.renv_finalizers
  applyFinalizer app.runapp_html_env.html_reactive_env finalizers
  removeBoundary app.runapp_boundary
