{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Decode.Internal where

import Control.Monad.IO.Class
import Control.Monad
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
    wrapFunc :: (JSVal -> JSM a) -> JSM JSVal
    wrapFunc f = toJSVal $ fun $
      \_ _ [arg1] -> liftIO . writeIORef ioRef <=< f $ arg1

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
    [ pure val
    , wrapFunc onNull
    , wrapFunc onBool
    , wrapFunc onNumber
    , wrapFunc onString
    , wrapFunc onArray
    , wrapFunc onObject ]

  syncPoint
  liftIO (readIORef ioRef)
