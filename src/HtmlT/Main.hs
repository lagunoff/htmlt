-- | Start and stop browser application
module HtmlT.Main where

import Data.IORef
import GHC.Generics

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
  } deriving Generic

-- | Needed to manually finalize and detach the application
data RunningApp = RunningApp
  { runapp_html_env :: HtmlEnv
  , runapp_boundary :: ContentBoundary
  } deriving Generic

-- | Most complete version of multiple functions that start the
-- application
attachOptions :: StartOpts -> Html a -> IO (a, RunningApp)
attachOptions StartOpts{..} render = mdo
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
  onBeforeUnload $
    readIORef (renv_finalizers startopts_reactive_env)
      >>= sequence_
  pure (result, runApp)

-- | Start the application and attach it to the given HTMLElement
attachTo :: DOMElement -> Html a -> IO (a, RunningApp)
attachTo rootEl html = do
  renv <- newReactiveEnv
  attachOptions (StartOpts renv rootEl) html

-- | Start the application and attach it to current <body> element
attachToBody :: Html a -> IO (a, RunningApp)
attachToBody html = do
  bodyEl <- getCurrentBody
  attachTo bodyEl html

-- | Run finalizers and detach created elements from the DOM
detach :: RunningApp -> IO ()
detach RunningApp{..} = do
  finalizers <- readIORef . renv_finalizers . html_reactive_env $
    runapp_html_env
  sequence_ finalizers
  removeBoundary runapp_boundary
