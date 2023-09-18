{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Utils where

import Control.Monad.IO.Class
import Data.Coerce
import GHC.JS.Foreign.Callback
import HtmlT
import JavaScript.Compat.Marshal
import JavaScript.Compat.String (JSString(..))
import Unsafe.Coerce

mkUrlHashRef :: MonadReactive m => m (DynRef JSString)
mkUrlHashRef = do
  initial <- liftIO js_readUrlHash
  routeRef <- newRef initial
  win <- liftIO getCurrentWindow
  popStateCb <- liftIO $ asyncCallback $
    js_readUrlHash >>= dynStep . writeRef routeRef
  liftIO $ js_setProp (coerce win) "onpopstate" (unsafeCoerce popStateCb)
  return routeRef

pushUrl :: MonadIO m => JSString -> m ()
pushUrl url = liftIO $ js_pushHref url

highlightHaskell :: JSString -> JSString
highlightHaskell = js_highlightHaskell

insertScript :: JSString -> IO ()
insertScript = js_insertScript

#if defined(javascript_HOST_ARCH)
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
foreign import javascript unsafe
  "(function(){\
    return location.hash;\
  })"
  js_readUrlHash :: IO JSString
foreign import javascript unsafe
  "(function(url){\
    return location.push(url);\
  })"
  js_pushHref :: JSString -> IO ()
#else
js_selectCountry :: DOMElement -> Nullable JSString -> IO () = errorGhcjsOnly
js_svgClickGetCountryCode :: DOMEvent -> IO (Nullable JSString) = errorGhcjsOnly
js_highlightHaskell :: JSString -> JSString = errorGhcjsOnly
js_insertScript :: JSString -> IO () = errorGhcjsOnly
js_readUrlHash :: IO JSString = errorGhcjsOnly
js_pushHref :: JSString -> IO () = errorGhcjsOnly
#endif
