{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Utils where

import Data.Text (Text)
import Clickable
import Debug.Trace

readLocalStorage :: FromJSVal v => Text -> JSM (Maybe v)
readLocalStorage key = do
  let jsonParse e = Call (Id "JSON") "parse" [e]
  val <- jsEval $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  traceShowM val
  return $ fromJSVal val

saveLocalStorage :: ToJSVal v => Text -> v -> JSM ()
saveLocalStorage key val = do
  let stringify s = Call (Id "JSON") "stringify" [toJSVal s]
  jsCmd $ Call (Id "localStorage") "setItem" [Str key, stringify val]

assignFocus :: RefId -> JSM ()
assignFocus elm = jsCmd $ Call (Ref elm) "focus" []
