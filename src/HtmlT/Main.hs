-- | Start and stop browser application
module HtmlT.Main where

import GHC.Generics
import HtmlT.DOM
import HtmlT.Event
import HtmlT.Types

data AttachOptions = AttachOptions
  { reactive_env :: ReactiveEnv
  -- ^ Typically the program should only have one instance of
  -- 'ReactiveEnv', so when there are multiple running 'HtmlT'
  -- applications, use this field to share existing 'ReactiveEnv'
  -- between all of them
  , root_element :: DOMElement
  -- ^ HTMLElement where to attach the elements created by the
  -- application
  , wait_document_load :: Bool
  -- ^ If True block IO action until main document is fully loaded
  , unload_call_finalizers :: Bool
  -- ^ If True run finalizers on @beforeunload@ event. This happens
  -- just before browser tab is closed, the code in finalizers should
  -- only consist of non-blocking IO
  } deriving Generic

-- | Needed to manually finalize and detach the application
data RunningApp = RunningApp
  { runapp_html_env :: HtmlEnv
  , runapp_reactive_env :: ReactiveEnv
  , runapp_boundary :: ContentBoundary
  } deriving Generic

-- | Start 'HtmlT' application applying customizations described by
-- AttachOptions argument
attachWithOptions :: AttachOptions -> Html a -> IO (a, RunningApp)
attachWithOptions opt app = mdo
  -- TODO: doesn't work with javascript-backend
  -- when wait_document_load
  --   js_waitDocumentLoad
  begin <- createComment "ContentBoundary {{"
  end <- createComment "}}"
  appendChild opt.root_element begin
  appendChild opt.root_element end
  reactiveEnv <- newReactiveEnv
  let
    boundary = ContentBoundary begin end
    htmlEnv = HtmlEnv
      { html_current_element = opt.root_element
      , html_content_boundary = Just boundary
      }
    runApp = RunningApp htmlEnv reactiveEnv boundary
  result <- launchReactiveT reactiveEnv $ execHtmlT htmlEnv app
  -- when opt.unload_call_finalizers $ onBeforeUnload $ do
  --   finalizers <- readIORef $ renv_finalizers reactive_env
  --   applyFinalizer reactive_env finalizers
  return (result, runApp)

-- | Start the application and attach it to the given HTMLElement
attachTo :: DOMElement -> Html a -> IO (a, RunningApp)
attachTo root html = do
  renv <- newReactiveEnv
  attachWithOptions (AttachOptions renv root True True) html

-- | Start the application and attach it to current <body> element
attachToBody :: Html a -> IO (a, RunningApp)
attachToBody html = do
  body <- getCurrentBody
  attachTo body html

-- | Run finalizers and detach created elements from the DOM
detach :: RunningApp -> IO ()
detach app = do
  launchReactiveT app.runapp_reactive_env $ freeScope app.runapp_reactive_env.scope
  removeBoundary app.runapp_boundary
