module Clickable.DOM where

import Control.Monad.Reader
import Data.Text (Text)
import Data.Foldable
import Data.Kind
import Data.Int
import GHC.Generics
import Unsafe.Coerce

import Clickable.Types
import Clickable.Protocol
import Clickable.Protocol.Value (Value, FromJSValue(..))
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
  { aquire_fn :: ResourceScope -> SourceId -> Expr
  , mk_callback :: callback -> Value -> ClickM ()
  }

connectResource :: ConnectResourceArgs callback -> callback -> ClickM ()
connectResource args k = reactive_ \scope s ->
  let
    callback :: Value -> ClickM ()
    callback = local (\e -> e {scope}) . args.mk_callback k
    sourceId = SourceId s.next_id
    newSub = (scope, sourceId, callback . unsafeCoerce)
    connectExpr = ConnectResource scope $ args.aquire_fn scope sourceId
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
  type EventListenerCb "keydown" = Int64 -> ClickM ()
  addEventListenerArgs = keyboardConnectArgs "keydown"

instance IsEventName "keyup" where
  type EventListenerCb "keyup" = Int64 -> ClickM ()
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

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
pointerConnectArgs :: Text -> ConnectResourceArgs (ClickM ())
pointerConnectArgs eventName = ConnectResourceArgs
  { aquire_fn = \scope sourceId ->
    Eval (normalEventWrapper eventName defaultEventListenerOptions)
      `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event
submitConnectArgs :: ConnectResourceArgs (ClickM ())
submitConnectArgs = ConnectResourceArgs
  { aquire_fn = \scope sourceId ->
    Eval (normalEventWrapper "submit" EventListenerOptions
    { prevent_default = True
    , stop_propagation = True
    }) `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputConnectArgs :: Text -> ConnectResourceArgs (Text -> ClickM ())
inputConnectArgs eventName  = ConnectResourceArgs
  { aquire_fn = \scope sourceId -> Eval
      ("(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  window.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => window.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})") `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromJSValue event) k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardConnectArgs :: Text -> ConnectResourceArgs (Int64 -> ClickM ())
keyboardConnectArgs eventName = ConnectResourceArgs
  { aquire_fn = \scope sourceId -> Eval (
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  window.addEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \  return () => window.removeEventListener('" <> UnsafeJavaScript eventName <> "', listener);\n\
      \})") `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromJSValue event) k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
focusConnectArgs :: Text -> ConnectResourceArgs (ClickM ())
focusConnectArgs eventName = ConnectResourceArgs
  { aquire_fn = \scope sourceId ->
    Eval (normalEventWrapper eventName defaultEventListenerOptions)
      `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeConnectArgs :: ConnectResourceArgs (Bool -> ClickM ())
checkboxChangeConnectArgs = ConnectResourceArgs
  { aquire_fn = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.checked);\n\
      \  }\n\
      \  window.addEventListener('change', listener);\n\
      \  return () => window.removeEventListener('change', listener);\n\
      \})" `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromJSValue event) k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeConnectArgs :: ConnectResourceArgs (Text -> ClickM ())
selectChangeConnectArgs = ConnectResourceArgs
  { aquire_fn = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  window.addEventListener('change', listener);\n\
      \  return () => window.removeEventListener('change', listener);\n\
      \})" `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromJSValue event) k
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

-- https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event
popstateConnectArgs :: ConnectResourceArgs (Location -> ClickM ())
popstateConnectArgs = ConnectResourceArgs
  { aquire_fn = \scope sourceId -> Eval
      "(function(target, haskellCb){\n\
      \  function listener(){\n\
      \    haskellCb(location);\n\
      \  }\n\
      \  target.addEventListener('popstate', listener);\n\
      \  return () => target.removeEventListener('popstate', listener);\n\
      \})" `Apply` [Arg 0 0, Lam (TriggerCallback sourceId (Arg 0 0))]
  , mk_callback = \k event -> forM_ (fromJSValue event) k
  }
