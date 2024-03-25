module Clickable.Main where

import GHC.Generics
import Clickable.FFI
import Clickable.Core
import Clickable.Types
import Clickable.Internal (newInternalEnv)
import Wasm.Compat.Prim

data AttachOptions = AttachOptions
  { internal_env :: InternalEnv
  , dom_builder :: JSVal
  } deriving Generic

-- | Needed to manually finalize and detach the application
data RunningApp = RunningApp
  { internal_env :: InternalEnv
  , dom_bracket :: JSVal
  } deriving Generic

attachWithOptions :: AttachOptions -> HtmlM a -> IO (a, RunningApp)
attachWithOptions opt app = do
  domBracket <- js_insertBrackets opt.dom_builder
  result <- launchHtmlM domBracket opt.internal_env app
  let runApp = RunningApp opt.internal_env domBracket
  return (result, runApp)

attach :: HtmlM a -> IO (a, RunningApp)
attach html = do
  body <- documentBody
  internalEnv <- newInternalEnv
  attachWithOptions (AttachOptions internalEnv body) html

detach :: RunningApp -> IO ()
detach app = do
  launchClickM app.internal_env $ freeScope True app.internal_env.scope
  js_removeBrackets app.dom_bracket
