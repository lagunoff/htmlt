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
module Clickable.HTML where

import Clickable.Internal
import Clickable.Types
import Control.Monad.Reader
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

el :: Text -> HTML a -> HTML a
el tagName child = HTML \s e -> do
  e.hte_send $ PushStack $ CreateElement tagName
  (r, _) <- child.unHTML Nothing e
  e.hte_send PopIns
  pure (r, s)
{-# INLINE el #-}

elns :: Text -> Text -> HTML a -> HTML a
elns ns tagName child = HTML \s e -> do
  e.hte_send $ PushStack $ CreateElementNS ns tagName
  (r, _) <- child.unHTML s e
  e.hte_send PopIns
  pure (r, s)
{-# INLINE elns #-}

text :: Text -> HTML ()
text content = HTML \s e -> do
  e.hte_send $ PushStack $ CreateTextNode content
  e.hte_send PopIns
  return ((), s)
{-# INLINE text #-}

dynText :: Dynamic Text -> HTML ()
dynText contentDyn = HTML \s e -> do
  c <- readVal contentDyn
  refId <- newRefId.unJSM e
  e.hte_send $ PushStack $ CreateTextNode c
  e.hte_send $ AssignRef refId (PeekStack 0)
  e.hte_send PopIns
  let k nval = JSM \e' ->
        e'.hte_send $ UpdateTextNode (Ref refId) nval
  (subscribe contentDyn k).unJSM e
  pure ((), s)
{-# INLINEABLE dynText #-}

property :: ToValue val => Text -> val -> HTML ()
property k v = HTML \s e -> do
  e.hte_send $ ElementProp (PeekStack 0) k $ toValue v
  pure ((), s)
{-# INLINE property #-}

dynProp :: ToValue val => Text -> Dynamic val -> HTML ()
dynProp propName dynVal = HTML \s e -> do
  (refId, s') <- saveStackTip.unHTML s e
  initVal <- readVal dynVal
  e.hte_send $ ElementProp (PeekStack 0) propName $ toValue initVal
  let k nval = JSM \e' ->
        e'.hte_send $ ElementProp (Ref refId) propName $ toValue nval
  unJSM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynProp #-}

attribute :: Text -> Text -> HTML ()
attribute k v = HTML \s e -> do
  e.hte_send $ ElementAttr (PeekStack 0) k v
  pure ((), s)
{-# INLINE attribute #-}

dynAttr :: Text -> Dynamic Text -> HTML ()
dynAttr propName dynVal = HTML \s e -> do
  (refId, s') <- saveStackTip.unHTML s e
  initVal <- readVal dynVal
  e.hte_send $ ElementAttr (PeekStack 0) propName initVal
  let k nval = JSM \e' ->
        e'.hte_send $ ElementAttr (Ref refId) propName nval
  unJSM (subscribe dynVal k) e
  pure ((), s')
{-# INLINE dynAttr #-}

toggleClass :: Text -> Dynamic Bool -> HTML ()
toggleClass className dynEnable = HTML \s e -> do
  (refId, s') <- saveStackTip.unHTML s e
  v <- readVal dynEnable
  let k enable = JSM \e' -> e'.hte_send
          if enable
            then ClassListAdd (Ref refId) className
            else ClassListRemove (Ref refId) className
  unJSM (k v) e
  unJSM (subscribe dynEnable k) e
  pure ((), s')
{-# INLINE toggleClass #-}

addEventListener :: FromValue a => (Event a -> Expr) -> (a -> JSM ()) -> JSM ()
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
  connectEventName :: EventListenerCb eventName -> JSM ()

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> HTML ()
on k = liftJSM $ connectEventName @eventName k

instance IsEventName "click" where
  type EventListenerCb "click" = JSM ()
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

attachHTML :: Expr -> HTML a -> JSM a
attachHTML rootEl contents = JSM \e -> do
  e.hte_send $ PushStack rootEl
  (r, _) <- contents.unHTML Nothing e
  e.hte_send PopStack
  pure r

attachToBody :: HTML a -> JSM a
attachToBody = attachHTML $ Id "document" `Dot` "body"

saveStackTip :: HTML RefId
saveStackTip = HTML \s e ->
  case s of
    Nothing -> do
      refId <- newRefId.unJSM e
      e.hte_send $ AssignRef refId $ PeekStack 0
      return (refId, Just refId)
    Just saved ->
      pure (saved, s)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

data Location = Location {
  -- | A string containing the protocol scheme of the URL, including
  -- the final ':'
  protocol :: Text,
  -- | A string containing the domain of the URL.
  hostname :: Text,
  -- | A string containing the port number of the URL.
  port :: Text,
  -- | A string containing an initial '/' followed by the path of the
  -- URL, not including the query string or fragment.
  pathname :: Text,
  -- | String containing a '?' followed by the parameters or
  -- "querystring" of the URL
  search :: Text,
  -- | String containing a '#' followed by the fragment identifier
  -- of the URL.
  hash :: Text
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromValue, ToValue)

-- https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event
popstateEvent :: Event Location -> Expr
popstateEvent (Event eventId) =
  Eval
    "(function(target, trigger){\n\
    \  function listener(){\n\
    \    trigger({\n\
    \      protocol: location.protocol,\n\
    \      hostname: location.hostname,\n\
    \      port: location.port,\n\
    \      pathname: location.pathname,\n\
    \      search: location.search,\n\
    \      hash: location.hash\n\
    \    });\n\
    \  }\n\
    \  target.addEventListener('popstate', listener);\n\
    \  return () => target.removeEventListener('popstate', listener);\n\
    \})" `Apply` [Id "window", Lam (TriggerEvent eventId (Arg 0))]
