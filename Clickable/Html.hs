{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module Clickable.Html where

import Clickable.Internal ( subscribe, readVal, reactive, newRefId, enqueueExpr, newRefIdOp )
import Clickable.Types
import Control.Monad.Reader
import Data.Binary qualified as Binary
import Data.Binary.Put (execPut)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

el :: Text -> ClickM a -> ClickM a
el tagName child = ClickM \e -> do
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateElement tagName
  r <- child.unClickM e
  e.hte_send $ execPut $ Binary.put PopIns
  pure r
{-# INLINE el #-}

text :: Text -> ClickM ()
text content = ClickM \e -> do
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateTextNode content
  e.hte_send $ execPut $ Binary.put $ PopIns
{-# INLINE text #-}

dynText :: DynVal Text -> ClickM ()
dynText contentDyn = ClickM \e -> do
  c <- readVal contentDyn
  refId <- newRefId.unClickM e
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateTextNode c
  e.hte_send $ execPut $ Binary.put $ AssignRef refId (PeekStack 0)
  e.hte_send $ execPut $ Binary.put $ PopIns
  (subscribe contentDyn \nval -> ClickM \_ ->
    e.hte_send $ execPut $ Binary.put $ UpdateTextNode (Ref refId) nval
    ).unClickM e
{-# INLINEABLE dynText #-}

prop :: Text -> Expr -> ClickM ()
prop k v = ClickM \e ->
  e.hte_send $ execPut $ Binary.put $ ElementProp (PeekStack 0) k v
{-# INLINE prop #-}

addEventListener :: (Event ValueExpr -> Expr) -> (ValueExpr -> ClickM ()) -> ClickM ()
addEventListener connectScript k = do
  e <- reactive \scope s ->
    let k' = local (\e -> e {hte_scope = scope}) . k
        eventId = EventId s.next_id
        (s', unSubRef) = newRefIdOp scope s {next_id = s.next_id + 1}
        newSub = SubscriptionSimple scope (unsafeFromEventId eventId) (k' . unsafeCoerce)
        newFin = CustomFinalizer scope $ enqueueExpr $ Apply (Ref unSubRef) Null
        s'' = s' {subscriptions = newSub : s.subscriptions, finalizers = newFin : s.finalizers}
     in (s'', AssignRef unSubRef (connectScript (Event eventId)))
  enqueueExpr e

class IsEventName eventName where
  type EventListenerCb eventName :: Type
  connectEventName :: EventListenerCb eventName -> ClickM ()

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> ClickM ()
on k = connectEventName @eventName k

instance IsEventName "click" where
  type EventListenerCb "click" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "click") (const k)

data EventListenerOptions = EventListenerOptions
  { prevent_default :: Bool
  , stop_propagation :: Bool
  } deriving stock (Generic, Show, Eq)

defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions
  { prevent_default = False
  , stop_propagation = False
  }

genericEvent :: EventListenerOptions -> Text -> Event ValueExpr -> Expr
genericEvent opt eventName (Event eventId) =
  Eval
    ("(function([target, trigger]){\n\
    \  function listener(event){\n\
    \    " <> preventDefaultStmt <> "\n\
    \    " <> stopPropagationStmt <> "\n\
    \    trigger();\n\
    \  }\n\
    \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \})") `Apply` Arr [PeekStack 0, Lam (TriggerEvent eventId Null)]
  where
    preventDefaultStmt = if opt.prevent_default then "event.preventDefault();" else ""
    stopPropagationStmt = if opt.stop_propagation then "event.stopPropagation();" else ""
