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
import GHCJS.Nullable
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

foreign import javascript unsafe
  "(function(el, code){\
    if (!code) return;\
    var svgGroup = el.querySelector('#' + code); if (!svgGroup) return;\
    var svgPaths = svgGroup instanceof SVGPathElement ? [svgGroup] : svgGroup.querySelectorAll('path');\
    for (var i = 0; i < svgPaths.length; i++) {\
      svgPaths[i].classList.add('selected');\
    }\
    svgGroup.parentElement.appendChild(svgGroup);\
  })($1, $2)"
  js_selectCountry :: Node -> Nullable JSString -> IO ()

foreign import javascript unsafe
  "(function(event){\
    for(var i = 0; i < event.path.length; i++){\
      /* <svg> immediate children contains the country code */\
      if (event.path[i] instanceof SVGSVGElement && event.path[i - 1]){\
        return event.path[i - 1].id;\
      }\
    }\
    return null;\
  })($1)"
  js_svgClickGetCountryCode :: DOMEvent -> IO (Nullable JSString)
