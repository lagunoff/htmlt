module HtmlT.Decode where

import Control.Applicative
import Data.Coerce
import Data.Text as T
import GHCJS.Prim
import Language.Javascript.JSaddle hiding (Result)

newtype Decoder a = Decoder
  {runDecoder :: JSVal -> JSM (Maybe a)}

decodeJSVal :: Decoder JSVal
decodeJSVal = Decoder (pure . pure)

decoder :: FromJSVal a => Decoder a
decoder = Decoder fromJSVal

decodeAt :: [Text] -> Decoder a -> Decoder a
decodeAt keys dec = Decoder (go keys) where
  go [] obj = runDecoder dec obj
  go (k:ks) obj = do
    izNull <- ghcjsPure (isNull obj)
    izUndefined <- ghcjsPure (isUndefined obj)
    if izNull || izUndefined
      then return Nothing
      else obj ! k >>= go ks

withDecoder
  :: MonadJSM m
  => Coercible domEvent JSVal
  => Decoder a
  -> (a -> m ()) -> domEvent -> m ()
withDecoder dec f (coerce -> jsval) =
  maybe (return ()) f =<<
    liftJSM (runDecoder dec jsval)
{-# INLINEABLE withDecoder #-}

instance Functor Decoder where
  fmap f (Decoder run) = Decoder (fmap (fmap f) . run)

instance Applicative Decoder where
  pure a = Decoder \_ -> return (Just a)
  (<*>) (Decoder mf) (Decoder ma) = Decoder f where
    f v = liftA2 (<*>) (mf v) (ma v)
