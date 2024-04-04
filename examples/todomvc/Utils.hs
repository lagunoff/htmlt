module Utils where

import Data.Text (Text)

import Clickable
import Clickable.Protocol
import Clickable.Protocol.Value (FromJSValue(..), ToJSValue(..))

readLocalStorage :: FromJSValue v => Text -> ClickM (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExpr $ jsonParse $ Call (Id "localStorage") "getItem" [String key]
  return $ fromJSValue jsval

saveLocalStorage :: ToJSValue v => Text -> v -> ClickM ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . valueToExpr . toJSValue
  enqueueExpr $ Call (Id "localStorage") "setItem" [String key, stringify val]

assignFocus :: VarId -> ClickM ()
assignFocus elem = enqueueExpr $ Call (Var elem) "focus" []
