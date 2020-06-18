{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.Decode where

import Control.Applicative
import Control.Monad
import Data.JSString
import GHCJS.Prim
import JavaScript.Array (JSArray)
import JavaScript.Object (Object)
import Language.Javascript.JSaddle hiding (Result)
import Massaraksh.Decode.Internal

newtype Decoder a = Decoder
  {runDecoder' :: SomeJVal -> JSM (Either JSString a)}

data JVal ty where
  JNull   :: JSVal -> JVal "null"
  JBool   :: JSVal -> JVal "boolean"
  JNumber :: JSVal -> JVal "number"
  JString :: JSVal -> JVal "string"
  JArray  :: JSVal -> JVal "array"
  JObject :: JSVal -> JVal "object"

data SomeJVal = forall ty. SomeJVal {fromSomeJVal :: JVal ty}

type family JS2Hask ty where
  JS2Hask "null"    = ()
  JS2Hask "boolean" = Bool
  JS2Hask "number"  = Double
  JS2Hask "string"  = JSString
  JS2Hask "array"   = JSArray
  JS2Hask "object"  = Object

js2Hask :: JVal ty -> JSM (JS2Hask ty)
js2Hask = \case
  JNull _   -> pure ()
  JBool v   -> fromJSValUnchecked v
  JNumber v -> fromJSValUnchecked v
  JString v -> fromJSValUnchecked v
  JArray v  -> pure (SomeJSArray v)
  JObject v -> pure (Object v)

instance PToJSVal (JVal ty) where
  pToJSVal = \case
    JNull v   -> v
    JBool v   -> v
    JNumber v -> v
    JString v -> v
    JArray v  -> v
    JObject v -> v

instance PToJSVal SomeJVal where
  pToJSVal (SomeJVal v) = pToJSVal v

typeof :: JSVal -> JSM SomeJVal
typeof = case_js
  (pure . SomeJVal . JNull) (pure . SomeJVal . JBool)
  (pure . SomeJVal . JNumber) (pure . SomeJVal . JString)
  (pure . SomeJVal . JArray) (pure . SomeJVal . JObject)

runDecoder :: Decoder a -> JSVal -> JSM (Either JSString a)
runDecoder d = runDecoder' d <=< typeof

parseAt :: [JSString] -> Decoder a -> Decoder a
parseAt keys (Decoder parse) = Decoder (go keys)
  where
    go []     obj = parse obj
    go (x:xs) obj = decoder' @Object obj >>= \case
      Left err  -> pure (Left err)
      Right obj -> do o <- obj ! x; go xs =<< typeof o

decoder :: HasDecoder a => Decoder a
decoder = Decoder decoder'

instance Functor Decoder where
  fmap f (Decoder p) = Decoder (fmap (fmap f) . p)

instance Applicative Decoder where
  pure a = Decoder \_ -> pure (pure a)
  (<*>) (Decoder mf) (Decoder ma) = Decoder \v -> liftA2 (<*>) (mf v) (ma v)

class HasDecoder a where
  decoder' :: SomeJVal -> JSM (Either JSString a)

instance HasDecoder JSString where
  decoder' = \case
    SomeJVal v@JString{} -> fmap Right (js2Hask v)
    SomeJVal _           -> pure (Left "Expected a string")

instance HasDecoder Bool where
  decoder' = \case
    SomeJVal v@JBool{} -> fmap Right (js2Hask v)
    SomeJVal _         -> pure (Left "Expected a boolean")

instance HasDecoder Double where
  decoder' = \case
    SomeJVal v@JNumber{} -> fmap Right (js2Hask v)
    SomeJVal _           -> pure (Left "Expected a number")

instance HasDecoder Int where
  decoder' = \case
    SomeJVal v@JNumber{} -> fmap (Right . floor) (js2Hask v)
    SomeJVal _           -> pure (Left "Expected a number")

instance HasDecoder JSArray where
  decoder' = \case
    SomeJVal v@JArray{} -> fmap Right (js2Hask v)
    SomeJVal _          -> pure (Left "Expected an array")

instance HasDecoder Object where
  decoder' = \case
    SomeJVal v@JObject{} -> fmap Right (js2Hask v)
    SomeJVal _          -> pure (Left "Expected an object")

instance HasDecoder a => HasDecoder (Maybe a) where
  decoder' = \case
    SomeJVal v@JNull{} -> pure (Right Nothing)
    rest               -> fmap (fmap Just) (decoder' rest)
