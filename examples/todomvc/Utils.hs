{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Utils where

import Control.Monad
import Control.Monad.Reader
import Data.Coerce
import Data.Typeable
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import HtmlT
import JavaScript.Compat.Marshal
import JavaScript.Compat.String (JSString(..))
import JavaScript.Compat.String qualified as JSS
import Unsafe.Coerce

mkUrlHashRef :: MonadReactive m => m (DynRef JSString)
mkUrlHashRef = do
  initial <- liftIO readUrlHash
  routeRef <- newRef initial
  win <- getCurrentWindow
  popStateCb <- liftIO $ asyncCallback $
    readUrlHash >>= dynStep . writeRef routeRef
  liftIO $ js_setProp (coerce win) "onpopstate" (unsafeCoerce popStateCb)
  return routeRef

readUrlHash :: IO JSString
readUrlHash =
  js_readUrlHash

localStorageSet :: forall a m. (MonadIO m, ToJSVal a, Typeable a) => a -> m ()
localStorageSet val =
  liftIO (toJSVal val >>= js_setItem key)
  where
    key = JSS.pack . show $ typeRepFingerprint $ typeRep (Proxy @a)

localStorageGet :: forall a m. (MonadIO m, FromJSVal a, Typeable a) => m (Maybe a)
localStorageGet = liftIO do
  mval <- nullableToMaybe <$> (js_getItem key)
  join <$> forM mval fromJSVal
  where
    key = JSS.pack . show . typeRepFingerprint $ typeRep (Proxy @a)

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe
  "(($1) => { setTimeout(function() {\
    var inputEl = $1.parentNode.parentNode.querySelector('input.edit');\
    inputEl.focus();\
  }, 0); })"
  js_todoItemInputFocus :: JSVal -> IO ()

foreign import javascript unsafe
  "(function(key, val){\
    localStorage.setItem(key, JSON.stringify(val));\
  })"
  js_setItem :: JSString -> JSVal -> IO ()

foreign import javascript unsafe
  "(function(key){\
    var itemText = localStorage.getItem(key);\
    return itemText ? JSON.parse(itemText) : null;\
  })"
  js_getItem :: JSString -> IO (Nullable JSVal)
foreign import javascript unsafe
  "(function(){\
    return location.hash;\
  })"
  js_readUrlHash :: IO JSString

-- Need this because GHC.JS.Prim.Internal.Build buildObjectI3 is
-- broken. The FFI declarations were not migrated from GHCJS style
-- properly, they throw ReferenceError: $1 is not defined
foreign import javascript unsafe
  "(function(k1, v1, k2, v2, k3, v3){\
    var res = {};\
    res[k1] = v1; res[k2] = v2; res[k3] = v3;\
    return res;\
  })"
  js_buildObjectI3 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal
#else
js_todoItemInputFocus :: JSVal -> IO () = errorGhcjsOnly
js_setItem :: JSString -> JSVal -> IO () = errorGhcjsOnly
js_getItem :: JSString -> IO (Nullable JSVal) = errorGhcjsOnly
js_readUrlHash :: IO JSString = errorGhcjsOnly
js_buildObjectI3 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal = errorGhcjsOnly
#endif
