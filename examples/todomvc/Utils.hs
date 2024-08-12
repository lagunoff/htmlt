module Utils where

import Data.Text (Text)
import Clickable

readLocalStorage :: FromValue v => Text -> ClickM (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExpr $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ fromValue jsval

saveLocalStorage :: ToValue v => Text -> v -> ClickM ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . valueToExpr . toValue
  enqueueExpr $ Call (Id "localStorage") "setItem" [Str key, stringify val]

assignFocus :: VarId -> ClickM ()
assignFocus elem = enqueueExpr $ Call (Var elem) "focus" []
