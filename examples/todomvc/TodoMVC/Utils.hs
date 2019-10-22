module TodoMVC.Utils where

import Control.Monad (void)
import Language.Javascript.JSaddle (maybeNullOrUndefined, jsg, js, jss, fun, (#), (<#), JSM, fromJSVal, toJSVal)
import Control.Lens hiding ((#))
import Massaraksh.Component
import qualified TodoMVC.Item as Item
import Data.Text (Text)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Aeson (toJSON, decodeStrict')
import qualified Data.Text.Encoding as T

setup
  :: (Text -> msg)
  -> msg
  -> AppHandle msg model
  -> JSM ()
setup hashChange beforeUnload handle = do
  win <- jsg "window"
  win ^. jss "onpopstate" (fun \_ _ _ -> do
    Just hash <- jsg "location" ^. js "hash" >>= fromJSVal
    appHandleSend handle $ hashChange hash)
    
  win ^. jss "onbeforeunload" (fun \_ _ _ -> do
    appHandleSend handle $ beforeUnload)

writeTodos :: [Item.Model] -> JSM ()
writeTodos xs = do
  value <- toJSVal (toJSON xs)
  stringValue <- jsg "JSON" # "stringify" $ [value]
  key <- toJSVal "todomvc-massaraksh"
  void $ jsg "localStorage" # "setItem" $ [key, stringValue]

readTodos :: JSM [Item.Model]
readTodos = maybe [] id <$> runMaybeT do
  strOrNull <- lift (jsg "localStorage" # "getItem" $ ["todomvc-massaraksh"])
  str <- MaybeT (maybeNullOrUndefined strOrNull)
  txt <- MaybeT (fromJSVal str)
  MaybeT $ pure $ decodeStrict' (T.encodeUtf8 txt)

writeHash :: Text -> JSM ()
writeHash =
  jsg "location" <# "href"

readHash :: JSM Text
readHash = do
  jsval <- jsg "location" ^. js "hash"
  maybe (T.pack "") id <$> fromJSVal jsval
