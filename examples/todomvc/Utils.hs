{-# LANGUAGE NoOverloadedStrings #-}
module Utils where

import Control.Monad (void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe
import Data.Text
import Language.Javascript.JSaddle
import Massaraksh
import qualified Item as Item

setup
  :: (Text -> msg)
  -> msg
  -> (msg -> JSM ())
  -> JSM ()
setup hashChange beforeUnload handle = do
  win <- jsg "window"
  win <# "onpopstate" $ fun \_ _ _ -> do
    Just hash <- jsg "location" ! "hash" >>= fromJSVal
    handle $ hashChange hash
  win <# "onbeforeunload" $ fun \_ _ _ -> do
    handle $ beforeUnload

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

unsafeInit :: HtmlT s m x -> m x
unsafeInit = flip runReaderT env . runHtmlT where
  env = error "unsafeInit: Illegal usage of `HtmlEnv`"
