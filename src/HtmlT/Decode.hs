module HtmlT.Decode where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Coerce
import Data.JSString.Text
import Data.Text as T
import GHCJS.Marshal
import GHCJS.Prim
import JavaScript.Object.Internal (Object(..))
import qualified JavaScript.Object as Object

-- | Extract some value from 'JSVal'. Name comes from the Elm
-- ecosystem and it's main purpose is to parse information from DOM
-- events (like mouse coordinates, keyboard keys, input values etc)
newtype Decoder a = Decoder {unDecoder :: JSVal -> IO (Maybe a)}

decodeJSVal :: Decoder JSVal
decodeJSVal = Decoder (pure . pure)

decoder :: FromJSVal a => Decoder a
decoder = Decoder fromJSVal

decodeAt :: [Text] -> Decoder a -> Decoder a
decodeAt keys dec = Decoder (go keys) where
  go [] obj = unDecoder dec obj
  go (k:ks) obj = do
    izNull <- return (isNull obj)
    izUndefined <- return (isUndefined obj)
    if izNull || izUndefined
      then return Nothing
      else Object.getProp (textToJSString k) (coerce obj) >>= go ks

withDecoder
  :: (MonadIO m, Coercible domEvent JSVal)
  => Decoder a
  -> (a -> m ()) -> domEvent -> m ()
withDecoder dec f (coerce -> jsval) =
  maybe (return ()) f =<< liftIO (unDecoder dec jsval)
{-# INLINEABLE withDecoder #-}

instance Functor Decoder where
  fmap f (Decoder run) = Decoder (fmap (fmap f) . run)

instance Applicative Decoder where
  pure a = Decoder \_ -> return (Just a)
  (<*>) (Decoder mf) (Decoder ma) = Decoder f where
    f v = liftA2 (<*>) (mf v) (ma v)
