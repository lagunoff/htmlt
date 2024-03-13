{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Utils where

import Control.Monad
import Control.Monad.Reader
import Data.Typeable
import HtmlT
import Wasm.Compat.Prim
import Wasm.Compat.Marshal

mkUrlHashRef :: MonadReactive m => m (DynRef JSString)
mkUrlHashRef = do
  initial <- liftIO js_readUrlHash
  routeRef <- newRef initial
  win <- getCurrentWindow
  popStateCb <- liftIO $ js_dynExport1 \_ ->
    js_readUrlHash >>= dynStep . writeRef routeRef
  liftIO $ js_addEventListener win ((\(JSString j) -> j) $ toJSString "onpopstate") popStateCb
  return routeRef

localStorageSet :: forall a m. (MonadIO m, ToJSVal a, Typeable a) => a -> m ()
localStorageSet val =
  liftIO (toJSVal val >>= js_setItem key)
  where
    key = toJSString . show $ typeRepFingerprint $ typeRep (Proxy @a)

localStorageGet :: forall a m. (MonadIO m, FromJSVal a, Typeable a) => m (Maybe a)
localStorageGet = liftIO do
  mval <- nullableToMaybe <$> (js_getItem key)
  liftIO $ js_consoleLog . (.unNullable) =<< js_getItem key
  join <$> forM mval fromJSVal
  where
    key = toJSString . show . typeRepFingerprint $ typeRep (Proxy @a)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe
  "setTimeout(function() {\
    var inputEl = $1.parentNode.parentNode.querySelector('input.edit');\
    inputEl.focus();\
  }, 0)"
  js_todoItemInputFocus :: JSVal -> IO ()

foreign import javascript unsafe
  "localStorage.setItem($1, JSON.stringify($2));"
  js_setItem :: JSString -> JSVal -> IO ()

foreign import javascript unsafe
  "console.log($1)"
  js_consoleLog :: JSVal -> IO ()

foreign import javascript unsafe
  "var itemText = localStorage.getItem($1);\
   return itemText ? JSON.parse(itemText) : null;"
  js_getItem :: JSString -> IO (Nullable JSVal)

foreign import javascript unsafe
  "location.hash"
  js_readUrlHash :: IO JSString

-- Need this because GHC.JS.Prim.Internal.Build buildObjectI3 is
-- broken. The FFI declarations were not migrated from GHCJS style
-- properly, they throw ReferenceError: $1 is not defined
foreign import javascript unsafe
  "(function(k1, v1, k2, v2, k3, v3){\
    var res = {};\
    res[k1] = v1; res[k2] = v2; res[k3] = v3;\
    return res;\
  })($1,$2,$3,$4,$5,$6)"
  js_buildObjectI3 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal
#else
js_todoItemInputFocus :: JSVal -> IO () = errorGhcjsOnly
js_setItem :: JSString -> JSVal -> IO () = errorGhcjsOnly
js_getItem :: JSString -> IO (Nullable JSVal) = errorGhcjsOnly
js_readUrlHash :: IO JSString = errorGhcjsOnly
js_buildObjectI3 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal = errorGhcjsOnly
#endif
