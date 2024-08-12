module Clickable.DOM where

import Control.Monad.Reader
import Data.Foldable
import Data.Int
import Data.Kind
import Data.Text (Text)
import GHC.Generics
import Unsafe.Coerce

import Clickable.Types
import Clickable.Protocol
import Clickable.Protocol.Value
import Clickable.Internal


data EventListenerOptions = EventListenerOptions
  { prevent_default :: Bool
  , stop_propagation :: Bool
  } deriving stock (Generic, Show, Eq)

defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions
  { prevent_default = False
  , stop_propagation = False
  }

addEventListener :: ConnectResourceArgs callback -> callback -> HtmlM ()
addEventListener args k = lift $ connectResource args k

data ConnectResourceArgs callback = ConnectResourceArgs
  { aquire_resource :: ResourceScope -> EventId -> Expr
  -- ^ When evaluated, as a side-effect resulting `Expr` must
  -- initialize some resource (could be DOM event, WebSocket
  -- connection etc) also must return a function that frees that
  -- resource
  , mk_callback :: callback -> Value -> ClickM ()
  }

connectResource :: ConnectResourceArgs callback -> callback -> ClickM ()
connectResource args k = reactive_ \scope s ->
  let
    k' :: Value -> ClickM ()
    k' = local (\e -> e {scope}) . args.mk_callback k
    event = unsafeFromEventId eventId
    eventId = EventId $ Int32Le s.next_id
    newSub = SubscriptionSimple scope event (k' . unsafeCoerce)
    connectExpr = ConnectResource scope $ args.aquire_resource scope eventId
  in
    s { evaluation_queue = connectExpr : s.evaluation_queue
      , subscriptions = newSub : s.subscriptions
      , next_id = s.next_id + 1
      }

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> HtmlM ()
on k = addEventListener (addEventListenerArgs @eventName) k

class IsEventName eventName where
  type EventListenerCb eventName :: Type
  addEventListenerArgs :: ConnectResourceArgs (EventListenerCb eventName)

instance IsEventName "click" where
  type EventListenerCb "click" = ClickM ()
  addEventListenerArgs = pointerConnectArgs "click"

instance IsEventName "mousedown" where
  type EventListenerCb "mousedown" = ClickM ()
  addEventListenerArgs = pointerConnectArgs "mousedown"

instance IsEventName "mouseup" where
  type EventListenerCb "mouseup" = ClickM ()
  addEventListenerArgs = pointerConnectArgs "mouseup"

instance IsEventName "dblclick" where
  type EventListenerCb "dblclick" = ClickM ()
  addEventListenerArgs = pointerConnectArgs "dblclick"

instance IsEventName "submit" where
  type EventListenerCb "submit" = ClickM ()
  addEventListenerArgs = submitConnectArgs

instance IsEventName "input" where
  type EventListenerCb "input" = Text -> ClickM ()
  addEventListenerArgs = inputConnectArgs "input"

instance IsEventName "keydown" where
  type EventListenerCb "keydown" = Int32 -> ClickM ()
  addEventListenerArgs = keyboardConnectArgs "keydown"

instance IsEventName "keyup" where
  type EventListenerCb "keyup" = Int32 -> ClickM ()
  addEventListenerArgs = keyboardConnectArgs "keyup"

instance IsEventName "focus" where
  type EventListenerCb "focus" = ClickM ()
  addEventListenerArgs = pointerConnectArgs "focus"

instance IsEventName "blur" where
  type EventListenerCb "blur" = ClickM ()
  addEventListenerArgs = pointerConnectArgs "blur"

instance IsEventName "input/blur" where
  type EventListenerCb "input/blur" = Text -> ClickM ()
  addEventListenerArgs = inputConnectArgs "blur"

instance IsEventName "input/focus" where
  type EventListenerCb "input/focus" = Text -> ClickM ()
  addEventListenerArgs = inputConnectArgs "focus"

instance IsEventName "checkbox/change" where
  type EventListenerCb "checkbox/change" = Bool -> ClickM ()
  addEventListenerArgs = checkboxChangeConnectArgs

instance IsEventName "select/change" where
  type EventListenerCb "select/change" = Text -> ClickM ()
  addEventListenerArgs = selectChangeConnectArgs

instance IsEventName "mousewheel" where
  type EventListenerCb "mousewheel" = MouseWheel -> ClickM ()
  addEventListenerArgs = mouseWheelConnectArgs

eventListenerOptions :: Text -> Bool -> Bool -> ConnectResourceArgs (ClickM ())
eventListenerOptions eventName preventDef stopProp = ConnectResourceArgs
  { aquire_resource = \scope sourceId ->
    Eval (normalEventWrapper eventName $ EventListenerOptions preventDef stopProp)
      `Apply` [Arg 0 0, Lam (TriggerCallback sourceId Null)]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
pointerConnectArgs :: Text -> ConnectResourceArgs (ClickM ())
pointerConnectArgs eventName = ConnectResourceArgs
  { aquire_resource = \scope sourceId ->
    Eval (normalEventWrapper eventName defaultEventListenerOptions)
      `Apply` [Arg 0 0, Lam (TriggerCallback sourceId Null)]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event
submitConnectArgs :: ConnectResourceArgs (ClickM ())
submitConnectArgs = ConnectResourceArgs
  { aquire_resource = \scope sourceId ->
    Eval (normalEventWrapper "submit" EventListenerOptions
    { prevent_default = True
    , stop_propagation = True
    }) `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputConnectArgs :: Text -> ConnectResourceArgs (Text -> ClickM ())
inputConnectArgs eventName = ConnectResourceArgs
  { aquire_resource = \scope sourceId -> Eval
      ("(function(target, haskellCb){\n\
      \  function listener(event){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => window.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})") `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromValue event) k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardConnectArgs :: Text -> ConnectResourceArgs (Int32 -> ClickM ())
keyboardConnectArgs eventName = ConnectResourceArgs
  { aquire_resource = \scope sourceId -> Eval (
      "(function(target, haskellCb){\n\
      \  function listener(event){\n\
      \    haskellCb(event.keyCode);\n\
      \  }\n\
      \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})") `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromValue event) k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
focusConnectArgs :: Text -> ConnectResourceArgs (ClickM ())
focusConnectArgs eventName = ConnectResourceArgs
  { aquire_resource = \scope sourceId ->
    Eval (normalEventWrapper eventName defaultEventListenerOptions)
      `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeConnectArgs :: ConnectResourceArgs (Bool -> ClickM ())
checkboxChangeConnectArgs = ConnectResourceArgs
  { aquire_resource = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(event){\n\
      \    haskellCb(event.target.checked);\n\
      \  }\n\
      \  target.addEventListener('change', listener);\n\
      \  return () => window.removeEventListener('change', listener);\n\
      \})" `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromValue event) k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeConnectArgs :: ConnectResourceArgs (Text -> ClickM ())
selectChangeConnectArgs = ConnectResourceArgs
  { aquire_resource = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(event){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  target.addEventListener('change', listener);\n\
      \  return () => target.removeEventListener('change', listener);\n\
      \})" `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromValue event) k
  }

normalEventWrapper :: Text -> EventListenerOptions -> UnsafeJavaScript
normalEventWrapper eventName opt =
  "(function(target, haskellCb){\n\
  \  function listener(event){\n\
  \    " <> preventDefaultStmt <> "\n\
  \    " <> stopPropagationStmt <> "\n\
  \    haskellCb(event);\n\
  \  }\n\
  \  target.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
  \  return () => target.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
  \})"
  where
    preventDefaultStmt = if opt.prevent_default then "event.preventDefault();" else ""
    stopPropagationStmt = if opt.stop_propagation then "event.stopPropagation();" else ""

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
popstateConnectArgs :: ConnectResourceArgs (Location -> ClickM ())
popstateConnectArgs = ConnectResourceArgs
  { aquire_resource = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(){\n\
      \    haskellCb({\n\
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
      \})" `Apply` [Id "window", Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromValue event) k
  }

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
mouseWheelConnectArgs :: ConnectResourceArgs (MouseWheel -> ClickM ())
mouseWheelConnectArgs = ConnectResourceArgs
  { mk_callback = \k j -> forM_ (fromValue j) k
  , aquire_resource = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(event){\n\
      \    haskellCb({\n\
      \      mw_delta_x: event.deltaX,\n\
      \      mw_delta_y: event.deltaY,\n\
      \      mw_delta_z: event.deltaZ,\n\
      \      mw_alt_key: event.altKey,\n\
      \      mw_ctrl_key: event.ctrlKey,\n\
      \      mw_meta_key: event.metaKey,\n\
      \      mw_shift_key: event.shiftKey\n\
      \    });\n\
      \  }\n\
      \  target.addEventListener('popstate', listener);\n\
      \  return () => target.removeEventListener('popstate', listener);\n\
      \})" `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  }
