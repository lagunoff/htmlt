{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Decode.Internal where

import Control.Monad.IO.Class
import Control.Monad
import Data.Foldable
import Data.Text
import GHC.IORef
import GHCJS.Prim
import Language.Javascript.JSaddle hiding (Result)

case_js
  :: forall a
  . (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> (JSVal -> JSM a)
  -> JSVal
  -> JSM a
case_js onNull onBool onNumber onString onArray onObject val = do
  ioRef <- liftIO $ newIORef (error "case_json: none of the callbacks were called")
  let
    wrapF :: (JSVal -> JSM a) -> JSM Function
    wrapF f = function $ \_ _ [arg1] -> liftIO . writeIORef ioRef <=< f $ arg1

  onNullF <- wrapF onNull
  onBoolF <- wrapF onBool
  onNumberF <- wrapF onNumber
  onStringF <- wrapF onString
  onArrayF <- wrapF onArray
  onObjectF <- wrapF onObject

  caseFunc <- eval @Text
    "(function(val, onNull, onBool, onNumber, onString, onArray, onObject) {\
      if (typeof(val) === 'boolean') { onBool(val); return; }\
      if (typeof(val) === 'number') { onNumber(val); return; }\
      if (typeof(val) === 'string') { onString(val); return; }\
      if (Array.isArray(val)) { onArray(val); return; }\
      if (val === null || val === undefined) { onNull(val); return; }\
      onObject(val);\
    })"

  call caseFunc global
    [ pure val, toJSVal onNullF, toJSVal onBoolF, toJSVal onNumberF
    , toJSVal onStringF, toJSVal onArrayF, toJSVal onObjectF ]

  for_ [onNullF, onBoolF, onNumberF, onStringF, onArrayF, onObjectF] freeFunction
  syncPoint
  liftIO (readIORef ioRef)
