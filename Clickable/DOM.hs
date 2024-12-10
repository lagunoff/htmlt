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
module Clickable.DOM where

import Clickable.Internal
import Clickable.Types
import Data.Int
import Data.Kind
import Data.Text (Text)
import GHC.Generics (Generic)
import Unsafe.Coerce


data ListenerOptions = ListenerOptions {
  prevent_default :: Bool,
  stop_propagation :: Bool
} deriving (Generic, Show, Eq)

defaultListenerOptions :: ListenerOptions
defaultListenerOptions = ListenerOptions {
  prevent_default = False,
  stop_propagation = False
}

addEventListener :: FromJSVal a => (Event a -> JSExp) -> (a -> JSM ()) -> JSM ()
addEventListener script k =
  reactive add >>= jsCmd where
    add scope s = (s''', cmd) where
      k' = localScope scope . k
      eventId = EventId s.ist_id_supply
      (s', unsub) = newRefIdFn scope s {ist_id_supply = s.ist_id_supply + 1}
      s'' = subscribeEventFn (unsafeFromEventId eventId)
        (mapM_ k' . fromJSVal . unsafeCoerce) scope s'
      s''' = installFinalizerFn (jsCmd $ Apply (Ref unsub) []) scope s''
      cmd = AssignRef scope unsub $ script $ Event eventId

class EventName eventName where
  type EventListenerCb eventName :: Type
  connectEventName :: JSExp -> EventListenerCb eventName -> JSM ()

instance EventName "click" where
  type EventListenerCb "click" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "click" t) (const k)

instance EventName "mousedown" where
  type EventListenerCb "mousedown" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "mousedown" t) (const k)

instance EventName "mouseup" where
  type EventListenerCb "mouseup" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "mouseup" t) (const k)

instance EventName "mouseenter" where
  type EventListenerCb "mouseenter" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "mouseenter" t) (const k)

instance EventName "mouseleave" where
  type EventListenerCb "mouseleave" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "mouseleave" t) (const k)

instance EventName "dblclick" where
  type EventListenerCb "dblclick" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "dblclick" t) (const k)

instance EventName "submit" where
  type EventListenerCb "submit" = JSM ()
  connectEventName t k =
    addEventListener (genericEvent opt "submit" t) (const k)
    where
      opt = ListenerOptions {
        prevent_default = True,
        stop_propagation = True
      }

instance EventName "input" where
  type EventListenerCb "input" = Text -> JSM ()
  connectEventName t = addEventListener (inputEvent t "input")

instance EventName "keydown" where
  type EventListenerCb "keydown" = Int32 -> JSM ()
  connectEventName t = addEventListener (keyboardEvent t "keydown")

instance EventName "keyup" where
  type EventListenerCb "keyup" = Int32 -> JSM ()
  connectEventName t = addEventListener (keyboardEvent t "keyup")

instance EventName "focus" where
  type EventListenerCb "focus" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "focus" t) (const k)

instance EventName "blur" where
  type EventListenerCb "blur" = JSM ()
  connectEventName t k = addEventListener
    (genericEvent defaultListenerOptions "blur" t) (const k)

instance EventName "input/blur" where
  type EventListenerCb "input/blur" = Text -> JSM ()
  connectEventName t = addEventListener (inputEvent t "blur")

instance EventName "input/focus" where
  type EventListenerCb "input/focus" = Text -> JSM ()
  connectEventName t = addEventListener (inputEvent t "focus")

instance EventName "checkbox/change" where
  type EventListenerCb "checkbox/change" = Bool -> JSM ()
  connectEventName t = addEventListener (checkboxChangeEvent t)

instance EventName "select/change" where
  type EventListenerCb "select/change" = Text -> JSM ()
  connectEventName t = addEventListener (selectChangeEvent t)

instance EventName "mousewheel" where
  type EventListenerCb "mousewheel" = MouseWheel -> JSM ()
  connectEventName t = addEventListener (mouseWheelEvent t)

on :: forall eventName. EventName eventName => EventListenerCb eventName -> HTML ()
on k = liftJSM $ connectEventName @eventName (PeekStack 0) k

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
genericEvent :: ListenerOptions -> Text -> JSExp -> Event () -> JSExp
genericEvent opt eventName target (Event eventId) =
  Eval script `Apply` [target, Lam (TriggerEvent eventId Null)]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(event){\n\
      \    " <> preventDefaultStmt <> "\n\
      \    " <> stopPropagationStmt <> "\n\
      \    trigger();\n\
      \  }\n\
      \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})"
    preventDefaultStmt = if opt.prevent_default then "event.preventDefault();" else ""
    stopPropagationStmt = if opt.stop_propagation then "event.stopPropagation();" else ""

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputEvent :: JSExp -> Text -> Event Text -> JSExp
inputEvent target eventName (Event eventId) =
  Eval script `Apply` [target, Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(event){\n\
      \    trigger(event.target.value);\n\
      \  }\n\
      \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})"

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardEvent :: JSExp -> Text -> Event Int32 -> JSExp
keyboardEvent target eventName (Event eventId) =
  Eval script `Apply` [target, Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(event){\n\
      \    trigger(event.keyCode);\n\
      \  }\n\
      \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})"

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeEvent :: JSExp -> Event Bool -> JSExp
checkboxChangeEvent target (Event eventId) =
  Eval script `Apply` [target, Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(event){\n\
      \    trigger(event.target.checked);\n\
      \  }\n\
      \  target.addEventListener('change', listener);\n\
      \  return () => target.removeEventListener('change', listener);\n\
      \})"

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeEvent :: JSExp -> Event Text -> JSExp
selectChangeEvent target (Event eventId) =
  Eval script `Apply` [target, Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(event){\n\
      \    trigger(event.target.value);\n\
      \  }\n\
      \  target.addEventListener('change', listener);\n\
      \  return () => target.removeEventListener('change', listener);\n\
      \})"

-- | Collection of deltaX, deltaY and deltaZ properties from WheelEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
data MouseWheel = MouseWheel {
  mw_delta_x :: Int32,
  mw_delta_y :: Int32,
  mw_delta_z :: Int32,
  mw_alt_key :: Bool,
  mw_ctrl_key :: Bool,
  mw_meta_key :: Bool,
  mw_shift_key :: Bool
} deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
mouseWheelEvent :: JSExp -> Event MouseWheel -> JSExp
mouseWheelEvent target (Event eventId) =
  Eval script `Apply` [target, Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(event){\n\
      \    trigger({\n\
      \      mw_delta_x: event.deltaX,\n\
      \      mw_delta_y: event.deltaY,\n\
      \      mw_delta_z: event.deltaZ,\n\
      \      mw_alt_key: event.altKey,\n\
      \      mw_ctrl_key: event.ctrlKey,\n\
      \      mw_meta_key: event.metaKey,\n\
      \      mw_shift_key: event.shiftKey\n\
      \    });\n\
      \  }\n\
      \  target.addEventListener('mousewheel', listener);\n\
      \  return () => target.removeEventListener('mousewheel', listener);\n\
      \})"

data Location = Location {
  -- | A string containing the protocol scheme of the URL, including
  -- the final ':'
  loc_protocol :: Text,
  -- | A string containing the domain of the URL.
  loc_hostname :: Text,
  -- | A string containing the port number of the URL.
  loc_port :: Text,
  -- | A string containing an initial '/' followed by the path of the
  -- URL, not including the query string or fragment.
  loc_pathname :: Text,
  -- | String containing a '?' followed by the parameters or
  -- "querystring" of the URL
  loc_search :: Text,
  -- | String containing a '#' followed by the fragment identifier
  -- of the URL.
  loc_hash :: Text
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

-- https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event
popstateEvent :: Event Location -> JSExp
popstateEvent (Event eventId) =
  Eval script `Apply` [Id "window", Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(target, trigger){\n\
      \  function listener(){\n\
      \    trigger({\n\
      \      loc_protocol: location.protocol,\n\
      \      loc_hostname: location.hostname,\n\
      \      loc_port: location.port,\n\
      \      loc_pathname: location.pathname,\n\
      \      loc_search: location.search,\n\
      \      loc_hash: location.hash\n\
      \    });\n\
      \  }\n\
      \  target.addEventListener('popstate', listener);\n\
      \  return () => target.removeEventListener('popstate', listener);\n\
      \})"
