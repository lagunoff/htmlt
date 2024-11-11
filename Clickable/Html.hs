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
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

el :: Text -> HtmlM a -> HtmlM a
el tagName child = HtmlM \s e -> do
  e.hte_send $ PushStack $ CreateElement tagName
  (r, _) <- child.unHtmlM Nothing e
  e.hte_send PopIns
  pure (r, s)
{-# INLINE el #-}

elns :: Text -> Text -> HtmlM a -> HtmlM a
elns ns tagName child = HtmlM \s e -> do
  e.hte_send $ PushStack $ CreateElementNS ns tagName
  (r, _) <- child.unHtmlM s e
  e.hte_send PopIns
  pure (r, s)
{-# INLINE elns #-}

text :: Text -> HtmlM ()
text content = HtmlM \s e -> do
  e.hte_send $ PushStack $ CreateTextNode content
  e.hte_send PopIns
  return ((), s)
{-# INLINE text #-}

dynText :: DynVal Text -> HtmlM ()
dynText contentDyn = HtmlM \s e -> do
  c <- readVal contentDyn
  refId <- newRefId.unClickM e
  e.hte_send $ PushStack $ CreateTextNode c
  e.hte_send $ AssignRef refId (PeekStack 0)
  e.hte_send PopIns
  let k nval = ClickM \e' ->
        e'.hte_send $ UpdateTextNode (Ref refId) nval
  (subscribe contentDyn k).unClickM e
  pure ((), s)
{-# INLINEABLE dynText #-}

property :: ToValue val => Text -> val -> HtmlM ()
property k v = HtmlM \s e -> do
  e.hte_send $ ElementProp (PeekStack 0) k $ toValue v
  pure ((), s)
{-# INLINE property #-}

dynProp :: ToValue val => Text -> DynVal val -> HtmlM ()
dynProp propName dynVal = HtmlM \s e -> do
  (refId, s') <- saveStackTip.unHtmlM s e
  initVal <- readVal dynVal
  e.hte_send $ ElementProp (PeekStack 0) propName $ toValue initVal
  let k nval = ClickM \e' ->
        e'.hte_send $ ElementProp (Ref refId) propName $ toValue nval
  unClickM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynProp #-}

attribute :: Text -> Text -> HtmlM ()
attribute k v = HtmlM \s e -> do
  e.hte_send $ ElementAttr (PeekStack 0) k v
  pure ((), s)
{-# INLINE attribute #-}

dynAttr :: Text -> DynVal Text -> HtmlM ()
dynAttr propName dynVal = HtmlM \s e -> do
  (refId, s') <- saveStackTip.unHtmlM s e
  initVal <- readVal dynVal
  e.hte_send $ ElementAttr (PeekStack 0) propName initVal
  let k nval = ClickM \e' ->
        e'.hte_send $ ElementAttr (Ref refId) propName nval
  unClickM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynAttr #-}

toggleClass :: Text -> DynVal Bool -> HtmlM ()
toggleClass className dynEnable = HtmlM \s e -> do
  (refId, s') <- saveStackTip.unHtmlM s e
  v <- readVal dynEnable
  let k enable = ClickM \e' -> e'.hte_send
          if enable
            then ClassListAdd (Ref refId) className
            else ClassListRemove (Ref refId) className
  unClickM (k v) e
  unClickM (subscribe dynEnable k) e
  pure ((), s')
{-# INLINE toggleClass #-}

addEventListener :: FromValue a => (Event a -> Expr) -> (a -> ClickM ()) -> ClickM ()
addEventListener connectScript k = do
  e <- reactive \scope s ->
    let k' = local (\e -> e {hte_scope = scope}) . k
        eventId = EventId s.next_id
        (s', unSubRef) = newRefIdOp scope s {next_id = s.next_id + 1}
        newSub = SubscriptionSimple scope (unsafeFromEventId eventId) (mapM_ k' . fromValue . unsafeCoerce)
        newFin = CustomFinalizer scope $ enqueueExpr $ Apply (Ref unSubRef) []
        s'' = s' {subscriptions = newSub : s.subscriptions, finalizers = newFin : s.finalizers}
     in (s'', AssignRef unSubRef (connectScript (Event eventId)))
  enqueueExpr e

class IsEventName eventName where
  type EventListenerCb eventName :: Type
  connectEventName :: EventListenerCb eventName -> ClickM ()

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> HtmlM ()
on k = liftC $ connectEventName @eventName k

instance IsEventName "click" where
  type EventListenerCb "click" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "click" (PeekStack 0)) (const k)

data EventListenerOptions = EventListenerOptions {
  prevent_default :: Bool,
  stop_propagation :: Bool
} deriving stock (Generic, Show, Eq)

defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions {
  prevent_default = False,
  stop_propagation = False
}

genericEvent :: EventListenerOptions -> Text -> Expr -> Event () -> Expr
genericEvent opt eventName target (Event eventId) =
  Eval
    ("(function(target, trigger){\n\
    \  function listener(event){\n\
    \    " <> preventDefaultStmt <> "\n\
    \    " <> stopPropagationStmt <> "\n\
    \    trigger();\n\
    \  }\n\
    \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \})") `Apply` [target, Lam (TriggerEvent eventId Null)]
  where
    preventDefaultStmt = if opt.prevent_default then "event.preventDefault();" else ""
    stopPropagationStmt = if opt.stop_propagation then "event.stopPropagation();" else ""

unsafeConnectEvent :: Expr -> UnsafeJavaScript -> Event a -> Expr
unsafeConnectEvent target ujs (Event eid) =
  Eval ujs `Apply` [target, Lam (TriggerEvent eid (Arg 0))]

attachHtml :: Expr -> HtmlM a -> ClickM a
attachHtml rootEl contents = ClickM \e -> do
  e.hte_send $ PushStack rootEl
  (r, _) <- contents.unHtmlM Nothing e
  e.hte_send PopStack
  pure r

attachToBody :: HtmlM a -> ClickM a
attachToBody = attachHtml $ Id "document" `Dot` "body"

saveStackTip :: HtmlM RefId
saveStackTip = HtmlM \s e ->
  case s of
    Nothing -> do
      refId <- newRefId.unClickM e
      e.hte_send $ AssignRef refId $ PeekStack 0
      return (refId, Just refId)
    Just saved ->
      pure (saved, s)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}
