{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Utils where

import Data.Coerce
import Control.Monad.IO.Class
import Data.JSString.Text
import Data.Text
import GHCJS.Foreign.Callback
import GHCJS.Types
import HtmlT
import JavaScript.Object.Internal
import qualified JavaScript.Object as Object
import qualified JavaScript.Web.Location as JS

mkUrlHashRef :: ReactiveEnv -> IO (DynRef Text)
mkUrlHashRef s = do
  initial <- readUrlHash
  routeRef <- runReactiveEnvT s (newRef initial)
  win <- getCurrentWindow
  popStateCb <- asyncCallback $
    readUrlHash >>= writeRef routeRef
  Object.setProp "onpopstate" (jsval popStateCb) (coerce win)
  return routeRef

readUrlHash :: IO Text
readUrlHash = do
  loc <- JS.getWindowLocation
  textFromJSString <$> JS.getHash loc

pushUrl :: MonadIO m => Text -> m ()
pushUrl url = liftIO do
  loc <- JS.getWindowLocation
  JS.setHref (textToJSString url) loc
