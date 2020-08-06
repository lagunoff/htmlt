{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.Decode where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Foldable
import Data.JSString as JSS
import Data.JSString.Text as JSS
import Data.Text as T
import GHC.IORef
import GHCJS.Prim
import JavaScript.Array (JSArray)
import JavaScript.Object (Object)
import Language.Javascript.JSaddle hiding (Result)

newtype Decoder a = Decoder
  {runDecoder :: JSVal -> JSM (Either Text a)}

decodeAt :: [JSString] -> Decoder a -> Decoder a
decodeAt keys (Decoder p) = Decoder (go keys) where
  go []     obj = p obj
  go (x:xs) obj = parseJSVal @Object obj >>= \case
    Left err  -> pure (Left err)
    Right obj -> do o <- obj ! x; go xs o

decodeJSVal :: Decoder JSVal
decodeJSVal = Decoder (pure . pure)

decoder :: HasDecoder a => Decoder a
decoder = Decoder parseJSVal
{-# INLINE decoder #-}

jsCase
  :: forall a
  . (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> JSVal
  -> JSM a
jsCase onNull onBool onNumber onString onArray onObject val = do
  ioRef <- liftIO $ newIORef (error "jsCase: none of the callbacks were called")
  let
    wrapF :: (JSVal -> JSM a) -> JSM Function
    wrapF f = function $ \_ _ [arg1] -> liftIO . writeIORef ioRef <=< f $ arg1

  onNullF <- wrapF onNull
  onBoolF <- wrapF onBool
  onNumberF <- wrapF onNumber
  onStringF <- wrapF onString
  onArrayF <- wrapF onArray
  onObjectF <- wrapF onObject

  caseFunc <- eval $ JSS.unlines [
    "(function(val, onNull, onBool, onNumber, onString, onArray, onObject) {",
    "  if (typeof(val) === 'boolean') { onBool(val); return; }",
    "  if (typeof(val) === 'number') { onNumber(val); return; }",
    "  if (typeof(val) === 'string') { onString(val); return; }",
    "  if (Array.isArray(val)) { onArray(val); return; }",
    "  if (val === null || val === undefined) { onNull(val); return; }",
    "  onObject(val);",
    "})" ]

  call caseFunc global
    [ pure val, toJSVal onNullF, toJSVal onBoolF, toJSVal onNumberF
    , toJSVal onStringF, toJSVal onArrayF, toJSVal onObjectF ]

  for_ [onNullF, onBoolF, onNumberF, onStringF, onArrayF, onObjectF] freeFunction
  syncPoint
  liftIO (readIORef ioRef)

jsCaseNull :: (JSVal -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseNull f = jsCase
  (fmap Right . f)
  (const (pure (Left "expected a null, got boolean")))
  (const (pure (Left "expected a null, got number")))
  (const (pure (Left "expected a null, got string")))
  (const (pure (Left "expected a null, got an array")))
  (const (pure (Left "expected a null, got an object")))

jsCaseBool :: (Bool -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseBool f = jsCase
  (const (pure (Left "expected a boolean, got null")))
  (fmap Right . f <=< fromJSValUnchecked)
  (const (pure (Left "expected a boolean, got number")))
  (const (pure (Left "expected a boolean, got string")))
  (const (pure (Left "expected a boolean, got an array")))
  (const (pure (Left "expected a boolean, got an object")))

jsCaseInt :: (Int -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseInt f = jsCase
  (const (pure (Left "expected a number, got null")))
  (const (pure (Left "expected a number, got boolean")))
  (fmap Right . f <=< fromJSValUnchecked)
  (const (pure (Left "expected a number, got string")))
  (const (pure (Left "expected a number, got an array")))
  (const (pure (Left "expected a number, got an object")))

jsCaseDouble :: (Double -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseDouble f = jsCase
  (const (pure (Left "expected a number, got null")))
  (const (pure (Left "expected a number, got boolean")))
  (fmap Right . f <=< fromJSValUnchecked)
  (const (pure (Left "expected a number, got string")))
  (const (pure (Left "expected a number, got an array")))
  (const (pure (Left "expected a number, got an object")))

jsCaseString :: (JSString -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseString f = jsCase
  (const (pure (Left "expected a string, got null")))
  (const (pure (Left "expected a string, got boolean")))
  (const (pure (Left "expected a string, got number")))
  (fmap Right . f <=< fromJSValUnchecked)
  (const (pure (Left "expected a string, got an array")))
  (const (pure (Left "expected a string, got an object")))

jsCaseArray :: (JSArray -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseArray f = jsCase
  (const (pure (Left "expected an array, got null")))
  (const (pure (Left "expected an array, got boolean")))
  (const (pure (Left "expected an array, got number")))
  (const (pure (Left "expected an array, got string")))
  (fmap Right . f . coerce @_ @JSArray)
  (const (pure (Left "expected an array, got an object")))

jsCaseObject :: (Object -> JSM a) -> JSVal -> JSM (Either Text a)
jsCaseObject f = jsCase
  (const (pure (Left "expected an object, got null")))
  (const (pure (Left "expected an object, got boolean")))
  (const (pure (Left "expected an object, got number")))
  (const (pure (Left "expected an object, got string")))
  (const (pure (Left "expected an object, got an array")))
  (fmap Right . f . coerce @_ @Object)

instance Functor Decoder where
  fmap f (Decoder p) = Decoder (fmap (fmap f) . p)

instance Applicative Decoder where
  pure a = Decoder \_ -> pure (pure a)
  (<*>) (Decoder mf) (Decoder ma) = Decoder \v -> liftA2 (<*>) (mf v) (ma v)

class HasDecoder a where
  parseJSVal :: JSVal -> JSM (Either Text a)

instance HasDecoder JSString where
  parseJSVal = jsCaseString pure

instance HasDecoder Text where
  parseJSVal = fmap (fmap JSS.textFromJSString) . jsCaseString pure

instance HasDecoder Bool where
  parseJSVal = jsCaseBool pure

instance HasDecoder Double where
  parseJSVal = jsCaseDouble pure

instance HasDecoder Int where
  parseJSVal = jsCaseInt pure

instance HasDecoder JSArray where
  parseJSVal = jsCaseArray pure

instance HasDecoder Object where
  parseJSVal = jsCaseObject pure

instance HasDecoder a => HasDecoder (Maybe a) where
  parseJSVal v = do
    ea <- parseJSVal @a v
    either (const $ jsCaseNull (const (pure Nothing)) v) (pure . Right . Just) ea
