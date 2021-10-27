{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Utils where

import Control.Monad.IO.Class
import Data.Coerce
import Data.JSString.Text
import Data.Text
import GHCJS.Foreign.Callback
import GHCJS.Nullable
import GHCJS.Types
import HtmlT
import JavaScript.Object.Internal
import qualified JavaScript.Object as Object
import qualified JavaScript.Web.Location as JS

mkUrlHashRef :: MonadReactive m => m (DynRef Text)
mkUrlHashRef = do
  initial <- liftIO readUrlHash
  routeRef <- newRef initial
  win <- liftIO getCurrentWindow
  popStateCb <- liftIO $ asyncCallback $
    readUrlHash >>= writeRef routeRef
  liftIO $ Object.setProp "onpopstate" (jsval popStateCb) (coerce win)
  return routeRef

readUrlHash :: IO Text
readUrlHash = do
  loc <- JS.getWindowLocation
  textFromJSString <$> JS.getHash loc

pushUrl :: MonadIO m => Text -> m ()
pushUrl url = liftIO do
  loc <- JS.getWindowLocation
  JS.setHref (textToJSString url) loc

highlightHaskell :: Text -> Text
highlightHaskell = textFromJSString . js_highlightHaskell . textToJSString

insertScript :: Text -> IO ()
insertScript = js_insertScript . textToJSString

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
  js_selectCountry :: DOMElement -> Nullable JSString -> IO ()

foreign import javascript unsafe
  "(function(event){\
    var iter = event.target;\
    for(;;){\
      if (!iter || !iter.parentNode) break;\
      /* <svg> immediate children contains the country code */\
      if (iter.parentNode instanceof SVGSVGElement) return iter.id;\
      iter = iter.parentNode;\
    }\
    return null;\
  })($1)"
  js_svgClickGetCountryCode :: DOMEvent -> IO (Nullable JSString)


foreign import javascript unsafe
  "Prism.highlight($1, Prism.languages.haskell, 'haskell')"
  js_highlightHaskell :: JSString -> JSString

foreign import javascript unsafe
  "(function(script){\
    var scriptEl = document.createElement('script');\
    scriptEl.innerText = script;\
    document.head.appendChild(scriptEl);\
  })($1)"
  js_insertScript :: JSString -> IO ()
