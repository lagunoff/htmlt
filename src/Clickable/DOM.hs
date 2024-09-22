module Clickable.DOM where

import Control.Monad.Reader
import Data.Int
import Data.Kind
import Data.Text (Text)
import GHC.Generics
import Unsafe.Coerce

import Clickable.Internal
import Clickable.Protocol
import Clickable.Protocol.Value
import Clickable.Types


data EventListenerOptions = EventListenerOptions
  { prevent_default :: Bool
  , stop_propagation :: Bool
  } deriving stock (Generic, Show, Eq)

defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions
  { prevent_default = False
  , stop_propagation = False
  }

-- | A snippet of JavaScript that subscribes to certain event. It
-- should return a function that revokes the subscribtion. The typical
-- usage will involve addEventListener/removeEventListener but it's
-- not limited to this use case. Same method can be used to establish
-- WebSocket connection or any other source of events in browser
newtype ConnectEventScript a = ConnectEventScript
  { unConnectEventScript :: Event a -> Expr
  }

addEventListener :: FromValue a => ConnectEventScript a -> (a -> ClickM ()) -> ClickM ()
addEventListener (ConnectEventScript ces) k = reactive_ \scope s ->
  let k' :: Value -> ClickM ()
      k' = local (\e -> e {scope}) . mapM_ k . fromValue
      eventId = EventId s.next_id
      newSub = SubscriptionSimple scope (unsafeFromEventId eventId) (k' . unsafeCoerce)
      connectExpr = ConnectResource scope $ ces $ unsafeFromEventId eventId
  in s { evaluation_queue = connectExpr : s.evaluation_queue
       , subscriptions = newSub : s.subscriptions
       , next_id = s.next_id + 1
       }

class IsEventName eventName where
  type EventListenerCb eventName :: Type
  connectEventName :: EventListenerCb eventName -> ClickM ()

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> HtmlM ()
on k = lift $ connectEventName @eventName k

instance IsEventName "click" where
  type EventListenerCb "click" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "click") (const k)

instance IsEventName "mousedown" where
  type EventListenerCb "mousedown" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "mousedown") (const k)

instance IsEventName "mouseup" where
  type EventListenerCb "mouseup" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "mouseup") (const k)

instance IsEventName "mouseenter" where
  type EventListenerCb "mouseenter" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "mouseenter") (const k)

instance IsEventName "mouseleave" where
  type EventListenerCb "mouseleave" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "mouseleave") (const k)

instance IsEventName "dblclick" where
  type EventListenerCb "dblclick" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "dblclick") (const k)

instance IsEventName "submit" where
  type EventListenerCb "submit" = ClickM ()
  connectEventName k = addEventListener (genericEvent opt "submit") (const k)
    where
      opt = EventListenerOptions
        { prevent_default = True
        , stop_propagation = True
        }

instance IsEventName "input" where
  type EventListenerCb "input" = Text -> ClickM ()
  connectEventName = addEventListener (inputEvent "input")

instance IsEventName "keydown" where
  type EventListenerCb "keydown" = Int32 -> ClickM ()
  connectEventName = addEventListener (keyboardEvent "keydown")

instance IsEventName "keyup" where
  type EventListenerCb "keyup" = Int32 -> ClickM ()
  connectEventName = addEventListener (keyboardEvent "keyup")

instance IsEventName "focus" where
  type EventListenerCb "focus" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "focus") (const k)

instance IsEventName "blur" where
  type EventListenerCb "blur" = ClickM ()
  connectEventName k = addEventListener
    (genericEvent defaultEventListenerOptions "blur") (const k)

instance IsEventName "input/blur" where
  type EventListenerCb "input/blur" = Text -> ClickM ()
  connectEventName = addEventListener (inputEvent "blur")

instance IsEventName "input/focus" where
  type EventListenerCb "input/focus" = Text -> ClickM ()
  connectEventName = addEventListener (inputEvent "focus")

instance IsEventName "checkbox/change" where
  type EventListenerCb "checkbox/change" = Bool -> ClickM ()
  connectEventName = addEventListener checkboxChangeEvent

instance IsEventName "select/change" where
  type EventListenerCb "select/change" = Text -> ClickM ()
  connectEventName = addEventListener selectChangeEvent

instance IsEventName "mousewheel" where
  type EventListenerCb "mousewheel" = MouseWheel -> ClickM ()
  connectEventName = addEventListener mouseWheelEvent

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
genericEvent :: EventListenerOptions -> Text -> ConnectEventScript ()
genericEvent opt eventName = ConnectEventScript \(Event eventId) ->
  Eval
    ("(function(target, trigger){\n\
    \  function listener(event){\n\
    \    " <> preventDefaultStmt <> "\n\
    \    " <> stopPropagationStmt <> "\n\
    \    trigger();\n\
    \  }\n\
    \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \})") `Apply` [AskDomBuilder, Lam (TriggerEvent eventId Null)]
  where
    preventDefaultStmt = if opt.prevent_default then "event.preventDefault();" else ""
    stopPropagationStmt = if opt.stop_propagation then "event.stopPropagation();" else ""

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputEvent :: Text -> ConnectEventScript Text
inputEvent eventName = ConnectEventScript \(Event eventId) ->
  Eval
    ("(function(target, trigger){\n\
    \  function listener(event){\n\
    \    trigger(event.target.value);\n\
    \  }\n\
    \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \})") `Apply` [AskDomBuilder, Lam (TriggerEvent eventId (Arg 0 0))]

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardEvent :: Text -> ConnectEventScript Int32
keyboardEvent eventName = ConnectEventScript \(Event eventId) ->
  Eval
    ("(function(target, trigger){\n\
    \  function listener(event){\n\
    \    trigger(event.keyCode);\n\
    \  }\n\
    \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
    \})") `Apply` [AskDomBuilder, Lam (TriggerEvent eventId (Arg 0 0))]

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeEvent :: ConnectEventScript Bool
checkboxChangeEvent = ConnectEventScript \(Event eventId) ->
  Eval
    "(function(target, trigger){\n\
    \  function listener(event){\n\
    \    trigger(event.target.checked);\n\
    \  }\n\
    \  target.addEventListener('change', listener);\n\
    \  return () => target.removeEventListener('change', listener);\n\
    \})" `Apply` [AskDomBuilder, Lam (TriggerEvent eventId (Arg 0 0))]

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeEvent :: ConnectEventScript Text
selectChangeEvent = ConnectEventScript \(Event eventId) ->
  Eval
    "(function(target, trigger){\n\
    \  function listener(event){\n\
    \    trigger(event.target.value);\n\
    \  }\n\
    \  target.addEventListener('change', listener);\n\
    \  return () => target.removeEventListener('change', listener);\n\
    \})" `Apply` [AskDomBuilder, Lam (TriggerEvent eventId (Arg 0 0))]

data Location = Location
  { protocol :: Text
  -- ^ A string containing the protocol scheme of the URL, including
  -- the final ':'
  , hostname :: Text
  -- ^ A string containing the domain of the URL.
  , port :: Text
  -- ^ A string containing the port number of the URL.
  , pathname :: Text
  -- ^ A string containing an initial '/' followed by the path of the
  -- URL, not including the query string or fragment.
  , search :: Text
  -- ^ A string containing a '?' followed by the parameters or
  -- "querystring" of the URL
  , hash :: Text
  -- ^ A string containing a '#' followed by the fragment identifier
  -- of the URL.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromValue, ToValue)

-- https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event
popstateEvent :: ConnectEventScript Location
popstateEvent = ConnectEventScript \(Event eventId) ->
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
    \})" `Apply` [Id "window", Lam (TriggerEvent eventId (Arg 0 0))]

-- | Collection of deltaX, deltaY and deltaZ properties from WheelEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
data MouseWheel = MouseWheel
  { mw_delta_x :: Int32
  , mw_delta_y :: Int32
  , mw_delta_z :: Int32
  , mw_alt_key :: Bool
  , mw_ctrl_key :: Bool
  , mw_meta_key :: Bool
  , mw_shift_key :: Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromValue, ToValue)

-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
mouseWheelEvent :: ConnectEventScript MouseWheel
mouseWheelEvent = ConnectEventScript \(Event eventId) ->
  Eval
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
    \})" `Apply` [AskDomBuilder, Lam (TriggerEvent eventId (Arg 0 0))]
