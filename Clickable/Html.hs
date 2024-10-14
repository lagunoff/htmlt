{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
module Clickable.Html where

import Clickable.Internal
import Clickable.Types
import Control.Monad.Reader
import Data.Binary qualified as Binary
import Data.Binary.Put (execPut)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

el :: Text -> HtmlM a -> HtmlM a
el tagName child = HtmlM \s e -> do
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateElement tagName
  r <- child.unHtmlM s e
  e.hte_send $ execPut $ Binary.put PopIns
  pure r
{-# INLINE el #-}

elns :: Text -> Text -> HtmlM a -> HtmlM a
elns ns tagName child = HtmlM \s e -> do
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateElementNS ns tagName
  r <- child.unHtmlM s e
  e.hte_send $ execPut $ Binary.put PopIns
  pure r
{-# INLINE elns #-}

text :: Text -> HtmlM ()
text content = HtmlM \s e -> do
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateTextNode content
  e.hte_send $ execPut $ Binary.put $ PopIns
  return ((), s)
{-# INLINE text #-}

dynText :: DynVal Text -> HtmlM ()
dynText contentDyn = HtmlM \s e -> do
  c <- readVal contentDyn
  refId <- newRefId.unClickM e
  e.hte_send $ execPut $ Binary.put $ PushStack $ CreateTextNode c
  e.hte_send $ execPut $ Binary.put $ AssignRef refId (PeekStack 0)
  e.hte_send $ execPut $ Binary.put $ PopIns
  let k nval = ClickM \e' ->
        e'.hte_send $ execPut $ Binary.put $ UpdateTextNode (Ref refId) nval
  (subscribe contentDyn k).unClickM e
  pure ((), s)
{-# INLINEABLE dynText #-}

prop :: ToValue val => Text -> val -> HtmlM ()
prop k v = HtmlM \s e -> do
  e.hte_send $ execPut $ Binary.put $ ElementProp (PeekStack 0) k $ toValue v
  pure ((), s)
{-# INLINE prop #-}

dynProp :: ToValue val => Text -> DynVal val -> HtmlM ()
dynProp propName dynVal = HtmlM \s e -> do
  (refId, s') <- saveStackTip.unHtmlM s e
  initVal <- readVal dynVal
  e.hte_send $ execPut $ Binary.put $ ElementProp (PeekStack 0) propName $ toValue initVal
  let k nval = ClickM \e' ->
        e'.hte_send $ execPut $ Binary.put $ ElementProp (Ref refId) propName $ toValue nval
  unClickM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynProp #-}

addEventListener :: (Event ValueExpr -> Expr) -> (ValueExpr -> ClickM ()) -> ClickM ()
addEventListener connectScript k = do
  e <- reactive \scope s ->
    let k' = local (\e -> e {hte_scope = scope}) . k
        eventId = EventId s.next_id
        (s', unSubRef) = newRefIdOp scope s {next_id = s.next_id + 1}
        newSub = SubscriptionSimple scope (unsafeFromEventId eventId) (k' . unsafeCoerce)
        newFin = CustomFinalizer scope $ enqueueExpr $ Apply (Ref unSubRef) []
        s'' = s' {subscriptions = newSub : s.subscriptions, finalizers = newFin : s.finalizers}
     in (s'', AssignRef unSubRef (connectScript (Event eventId)))
  enqueueExpr e

class IsEventName eventName where
  type EventListenerCb eventName :: Type
  connectEventName :: EventListenerCb eventName -> ClickM ()

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> HtmlM ()
on k = liftClick $ connectEventName @eventName k

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
    ("(function(target, trigger){\n\
    \  function listener(event){\n\
    \    " <> preventDefaultStmt <> "\n\
    \    " <> stopPropagationStmt <> "\n\
    \    trigger();\n\
    \  }\n\
    \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \})") `Apply` [PeekStack 0, Lam (TriggerEvent eventId Null)]
  where
    preventDefaultStmt = if opt.prevent_default then "event.preventDefault();" else ""
    stopPropagationStmt = if opt.stop_propagation then "event.stopPropagation();" else ""

attachHtml :: Expr -> HtmlM a -> ClickM a
attachHtml rootEl contents = ClickM \e -> do
  e.hte_send $ execPut $ Binary.put $ PushStack rootEl
  (r, _) <- contents.unHtmlM Nothing e
  e.hte_send $ execPut $ Binary.put PopIns
  pure r

attachToBody :: HtmlM a -> ClickM a
attachToBody = attachHtml $ Id "document" `Dot` "body"

saveStackTip :: HtmlM RefId
saveStackTip = HtmlM \s e ->
  case s of
    Nothing -> do
      refId <- newRefId.unClickM e
      e.hte_send $ execPut $ Binary.put $ AssignRef refId $ PeekStack 0
      return (refId, Just refId)
    Just saved ->
      pure (saved, s)
