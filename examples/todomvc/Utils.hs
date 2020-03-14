{-# LANGUAGE NoOverloadedStrings #-}
module Utils where

import Control.Monad (void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe
import Data.Text
import Language.Javascript.JSaddle
import qualified Item as Item

setup
  :: (msg -> IO ())
  -> msg
  -> (Text -> msg)
  -> JSM ()
setup handle beforeUnload hashChange = do
  win <- jsg "window"
  win <# "onpopstate" $ fun \_ _ _ -> do
    Just hash <- jsg "location" ! "hash" >>= fromJSVal
    liftIO $ handle $ hashChange hash
  win <# "onbeforeunload" $ fun \_ _ _ -> do
    liftIO $ handle $ beforeUnload

writeTodos :: [Item.Model] -> JSM ()
writeTodos xs = do
  value <- toJSVal xs
  stringValue <- jsg "JSON" # "stringify" $ value
  key <- toJSVal "todomvc-massaraksh"
  void $ jsg "localStorage" # "setItem" $ (key, stringValue)

readTodos :: JSM [Item.Model]
readTodos = fromMaybe [] <$> runMaybeT do
  strOrNull <- lift (jsg "localStorage" # "getItem" $ ["todomvc-massaraksh"])
  strValue <- MaybeT (maybeNullOrUndefined strOrNull)
  value <- lift (jsg "JSON" # "parse" $ strValue)
  MaybeT (fromJSVal value)

writeHash :: Text -> JSM ()
writeHash =
  jsg "location" <# "href"

readHash :: JSM Text
readHash = do
  jsval <- jsg "location" ! "hash"
  fromMaybe mempty <$> fromJSVal jsval
