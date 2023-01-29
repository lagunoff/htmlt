{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Utils where

import Control.Monad.Reader
import Data.Coerce
import Data.JSString.Text
import Data.JSString as JSS
import Data.Text
import Data.Typeable
import GHCJS.Foreign.Callback
import GHCJS.Marshal
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
  win <- getCurrentWindow
  popStateCb <- liftIO $ asyncCallback $
    readUrlHash >>= transactionWrite routeRef
  liftIO $ Object.setProp "onpopstate" (jsval popStateCb) (coerce win)
  return routeRef

readUrlHash :: IO Text
readUrlHash = do
  loc <- JS.getWindowLocation
  textFromJSString <$> JS.getHash loc

localStorageSet :: forall a m. (MonadIO m, ToJSVal a, Typeable a) => a -> m ()
localStorageSet val = liftIO (toJSVal val >>= js_setItem key) where
  key = JSS.pack $ show $ typeRepFingerprint $ typeRep (Proxy @a)

localStorageGet :: forall a m. (MonadIO m, FromJSVal a, Typeable a) => m (Maybe a)
localStorageGet = liftIO do
  mval <- nullableToMaybe <$> (js_getItem key)
  join <$> forM mval fromJSVal
  where
    key = JSS.pack $ show $ typeRepFingerprint $ typeRep (Proxy @a)

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "setTimeout(function() {\
    var inputEl = $1.parentNode.parentNode.querySelector('input.edit');\
    inputEl.focus();\
  }, 0)"
  js_todoItemInputFocus :: JSVal -> IO ()

foreign import javascript unsafe
  "(function(key, val){\
    localStorage.setItem(key, JSON.stringify(val));\
  })($1,$2)"
  js_setItem :: JSString -> JSVal -> IO ()

foreign import javascript unsafe
  "(function(key){\
    var itemText = localStorage.getItem(key);\
    return itemText ? JSON.parse(itemText) : null;\
  })($1)"
  js_getItem :: JSString -> IO (Nullable JSVal)
#else
js_todoItemInputFocus :: JSVal -> IO () = errorGhcjsOnly
js_setItem :: JSString -> JSVal -> IO () = errorGhcjsOnly
js_getItem :: JSString -> IO (Nullable JSVal) = errorGhcjsOnly
#endif
