{-# LANGUAGE NoOverloadedStrings #-}
module Todo.Utils where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe
import Data.Text
import Language.Javascript.JSaddle
import HtmlT

type (~>) a b = forall x. a x -> b x

type Component msg = (msg ~> HtmlT) -> (msg ~> HtmlT)

fix1 :: (w ~> m -> w ~> m) -> w ~> m
fix1 f = f (fix1 f)

compose1
  :: (w ~> m -> w ~> m)
  -> (w ~> m -> w ~> m)
  -> w ~> m -> w ~> m
compose1 a b wm = a (b wm)
{-# INLINE compose1 #-}

(<**>) :: DynRef a -> DynRef b -> DynRef (a, b)
(<**>) (DynRef dA aMod) (DynRef bDyn bMod) = DynRef dyn mod where
  dyn = (,) <$> dA <*> bDyn
  mod = \f -> do
    oldA <- liftIO $ dynamic_read dA
    oldB <- liftIO $ dynamic_read bDyn
    let (newA, newB) = f (oldA, oldB)
    aMod \_ -> newA
    bMod \_ -> newB

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

writeTodos :: ToJSVal item => [item] -> JSM ()
writeTodos xs = do
  value <- toJSVal xs
  stringValue <- jsg "JSON" # "stringify" $ value
  key <- toJSVal "todomvc-htmlt"
  void $ jsg "localStorage" # "setItem" $ (key, stringValue)

readTodos :: FromJSVal item => JSM [item]
readTodos = fromMaybe [] <$> runMaybeT do
  strOrNull <- lift (jsg "localStorage" # "getItem" $ ["todomvc-htmlt"])
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
