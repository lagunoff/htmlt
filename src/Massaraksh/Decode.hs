{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Decode where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text
import GHC.IORef
import GHC.Generics
import GHCJS.Prim
import JavaScript.Array (JSArray)
import JavaScript.Object (Object)
import Language.Javascript.JSaddle hiding (Result)

type Result a = JSM (Either Text a)

newtype Decoder a = Decoder { runDecoder :: JSVal -> Result a }

instance Functor Decoder where
  fmap f (Decoder parse) = Decoder \v -> fmap (fmap f) (parse v)

instance Applicative Decoder where
  pure a = Decoder \_ -> pure (pure a)
  (<*>) (Decoder mf) (Decoder ma) = Decoder \v -> liftA2 (<*>) (mf v) (ma v)

foldJs :: (() -> a) -> (Bool -> a) -> (Double -> a) -> (Text -> a) -> (JSArray -> a) -> (Object -> a) -> JSVal -> JSM a
foldJs = fold_json

foldJsNull :: a -> (() -> a) -> JSVal -> JSM a
foldJsNull def func =
  foldJs func (const def) (const def) (const def) (const def) (const def)

foldJsBool :: a -> (Bool -> a) -> JSVal -> JSM a
foldJsBool def func =
  foldJs (const def) func (const def) (const def) (const def) (const def)

foldJsNumber :: a -> (Double -> a) -> JSVal -> JSM a
foldJsNumber def func =
  foldJs (const def) (const def) func (const def) (const def) (const def)

foldJsString :: a -> (Text -> a) -> JSVal -> JSM a
foldJsString def func =
  foldJs (const def) (const def) (const def) func (const def) (const def)

foldJsArray :: a -> (JSArray -> a) -> JSVal -> JSM a
foldJsArray def func =
  foldJs (const def) (const def) (const def) (const def) func (const def)

foldJsObject :: a -> (Object -> a) -> JSVal -> JSM a
foldJsObject def func =
  foldJs (const def) (const def) (const def) (const def) (const def) func

foldAt :: [Text] -> Decoder a -> Decoder a
foldAt keys (Decoder parse) = Decoder \val -> go keys val
  where
    go []     obj = parse obj
    go (x:xs) obj = decodeJs @Object obj >>= \case
      Left err  -> pure (Left err)
      Right obj -> do o <- obj ! x; go xs o

isNull :: JSVal -> JSM Bool
isNull = foldJsNull False (\_ -> True)

isBool :: JSVal -> JSM Bool
isBool = foldJsBool False (\_ -> True)

isTrue :: JSVal -> JSM Bool
isTrue = foldJsBool False id

isFalse :: JSVal -> JSM Bool
isFalse = foldJsBool False not

isNumber :: JSVal -> JSM Bool
isNumber = foldJsNumber False (\_ -> True)

isString :: JSVal -> JSM Bool
isString = foldJsString False (\_ -> True)

isArray :: JSVal -> JSM Bool
isArray = foldJsArray False (\_ -> True)

isObject :: JSVal -> JSM Bool
isObject = foldJsObject False (\_ -> True)

class DecodeJs a where
  decodeJs :: JSVal -> Result a

decoder :: DecodeJs a => Decoder a
decoder = Decoder decodeJs

decodeMaybe :: DecodeJs a => JSVal -> JSM (Maybe a)
decodeMaybe = fmap (either (const Nothing) Just) . decodeJs

instance DecodeJs Text where
  decodeJs = foldJsString (Left "Not a String.") Right

instance DecodeJs Bool where
  decodeJs = foldJsBool (Left "Not a Bool.") Right

instance DecodeJs Double where
  decodeJs = foldJsNumber (Left "Not a Number.") Right

instance DecodeJs Int where
  decodeJs = foldJsNumber (Left "Not a Number.") (Right . floor)

instance DecodeJs JSArray where
  decodeJs = foldJsArray (Left "Not an Array.") Right

instance DecodeJs Object where
  decodeJs = foldJsObject (Left "Not an Object.") Right

valueDecoder :: Decoder Text
valueDecoder =
  foldAt ["target", "value"] (decoder @Text)

data DeltaMouse = DeltaMouse
  { deltaX :: Int
  , deltaY :: Int
  , deltaZ :: Int
  } deriving (Eq, Show, Generic)

deltaDecoder :: Decoder DeltaMouse
deltaDecoder = DeltaMouse
  <$> foldAt ["deltaX"] decoder
  <*> foldAt ["deltaY"] decoder
  <*> foldAt ["deltaZ"] decoder

targetDecoder :: Decoder JSVal
targetDecoder =
  foldAt ["target"] idDecoder

emptyDecoder :: Decoder ()
emptyDecoder =
  Decoder $ \_ -> pure (pure ())

pureDecoder :: a -> Decoder a
pureDecoder a =
  Decoder \_ -> pure (pure a)

idDecoder :: Decoder JSVal
idDecoder =
  Decoder $ pure . pure

checkedDecoder :: Decoder Bool
checkedDecoder =
  foldAt ["target", "checked"] (decoder @Bool)

keycodeDecoder :: Decoder Int
keycodeDecoder =
  foldAt ["keyCode"] (decoder @Int)

-- FIXME: Add GHCJS-FFI version of 'fold_json'
fold_json :: forall a. (() -> a) -> (Bool -> a) -> (Double -> a) -> (Text -> a) -> (JSArray -> a) -> (Object -> a) -> JSVal -> JSM a
fold_json onNull onBool onNumber onString onArray onObject val = do
  ioRef <- liftIO $ newIORef (error "fold_json: none of the callbacks were called")
  let
    wrapFunc :: (JSVal -> JSM a) -> JSM JSVal
    wrapFunc f = toJSVal $ fun $
      \ _ _ [arg1] -> liftIO . writeIORef ioRef =<< f arg1

  foldFunc <- eval @Text
    "(function(val, onNull, onBool, onNumber, onString, onArray, onObject) {\
      if (typeof(val) === 'boolean') { onBool(val); return; }\
      if (typeof(val) === 'number') { onNumber(val); return; }\
      if (typeof(val) === 'string') { onString(val); return; }\
      if (Array.isArray(val)) { onArray(val); return; }\
      if (val === null || val === undefined) { onNull(val); return; }\
      onObject(val);\
    })"

  call foldFunc global
    [ pure val
    , wrapFunc ((onNull <$>) . fromJSValUnchecked)
    , wrapFunc ((onBool <$>) . fromJSValUnchecked)
    , wrapFunc ((onNumber <$>) . fromJSValUnchecked)
    , wrapFunc ((onString <$>) . fromJSValUnchecked)
    , wrapFunc (pure . onArray . SomeJSArray)
    , wrapFunc (pure . onObject . Object) ]

  syncPoint
  liftIO (readIORef ioRef)
