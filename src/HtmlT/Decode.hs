module HtmlT.Decode where

import Control.Applicative
import Data.Coerce
import Data.JSString.Text
import Data.Text as T
import GHCJS.Marshal
import GHCJS.Prim
import JavaScript.Object.Internal (Object(..))
import qualified JavaScript.Object as Object

-- | Extract some value from 'JSVal'. Similar to decoders from Elm
-- core library. Typicaly they will be used to parse information from
-- DOM events (like mouse coordinates, keyboard keys, input values
-- etc)
newtype Decoder a = Decoder {runDecoder :: JSVal -> IO (Maybe a)}

decodeJSVal :: Decoder JSVal
decodeJSVal = Decoder (pure . pure)

decoder :: FromJSVal a => Decoder a
decoder = Decoder fromJSVal

decodeAt :: [Text] -> Decoder a -> Decoder a
decodeAt keys dec = Decoder (go keys) where
  go [] obj = runDecoder dec obj
  go (k:ks) obj
    | isNull obj || isUndefined obj = return Nothing
    | otherwise =
      Object.getProp (textToJSString k) (coerce obj) >>= go ks

instance Functor Decoder where
  fmap f (Decoder d) = Decoder (fmap (fmap f) . d)

instance Applicative Decoder where
  pure = Decoder . const . pure . pure
  (<*>) (Decoder mf) (Decoder ma) =
    Decoder \v -> liftA2 (<*>) (mf v) (ma v)
