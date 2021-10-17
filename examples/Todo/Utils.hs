{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Todo.Utils where

import Control.Monad.Reader
import Control.Applicative
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

(<**>) :: DynRef a -> DynRef b -> DynRef (a, b)
(<**>) (DynRef aDyn aMod) (DynRef bDyn bMod) = DynRef
  (liftA2 (,) aDyn bDyn)
  (\f -> do
    oldA <- liftIO $ dynamic_read aDyn
    oldB <- liftIO $ dynamic_read bDyn
    let (newA, newB) = f (oldA, oldB)
    aMod \_ -> newA
    bMod \_ -> newB)

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

localStorageSet :: forall a m. (MonadIO m, ToJSVal a, Typeable a) => a -> m ()
localStorageSet val = liftIO (toJSVal val >>= js_setItem key) where
  key = JSS.pack $ show $ typeRepFingerprint $ typeRep (Proxy @a)

localStorageGet :: forall a m. (MonadIO m, FromJSVal a, Typeable a) => m (Maybe a)
localStorageGet = liftIO do
  mval <- nullableToMaybe <$> (js_getItem key)
  join <$> forM mval fromJSVal
  where
    key = JSS.pack $ show $ typeRepFingerprint $ typeRep (Proxy @a)

foreign import javascript unsafe "$1.focus()" js_focus :: JSVal -> IO ()

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
