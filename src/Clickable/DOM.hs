module Clickable.DOM where

import Control.Monad
import Data.Bifunctor
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.State
import Data.Foldable
import Data.Function ((&))
import Data.IORef
import Data.List qualified as List
import Data.Text (Text)
import Data.Tuple
import Data.Typeable
import Data.Map (Map)
import Data.Set (Set)
import Data.Kind
import Data.Map qualified as Map
import GHC.Generics
import GHC.Exts hiding (build)
import GHC.TypeLits
import Unsafe.Coerce

import Clickable.FFI qualified as  FFI
import Clickable.Core
import Wasm.Compat.Prim
import Wasm.Compat.Marshal


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
addEventListener args k = do
  rootElement <- ask
  liftClickM $ connectResource rootElement args k

data ConnectResourceArgs callback = ConnectResourceArgs
  { js_wrapper :: RawJavaScript
  , mk_callback :: callback -> JSVal -> ClickM ()
  }

connectResource :: JSVal -> ConnectResourceArgs callback -> callback -> ClickM ()
connectResource target args k = do
  env <- ask
  let hsCallback' = (`runReaderT` env) . unClickM . args.mk_callback k
  hsCallback'' <- liftIO $ FFI.js_dynExport hsCallback'
  cancel <- liftIO $ FFI.aquireResource target args.js_wrapper.unRawJavaScript hsCallback''
  installFinalizer do
    liftIO $ FFI.apply0 cancel
    liftIO $ freeJSVal hsCallback''

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
  type EventListenerCb "keydown" = Int -> ClickM ()
  addEventListenerArgs = keyboardConnectArgs "keydown"

instance IsEventName "keyup" where
  type EventListenerCb "keyup" = Int -> ClickM ()
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
  { js_wrapper = normalEventWrapper eventName defaultEventListenerOptions
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event
submitConnectArgs :: ConnectResourceArgs (ClickM ())
submitConnectArgs = ConnectResourceArgs
  { js_wrapper = normalEventWrapper "submit" EventListenerOptions
    { prevent_default = True
    , stop_propagation = True
    }
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputConnectArgs :: Text -> ConnectResourceArgs (Text -> ClickM ())
inputConnectArgs eventName  = ConnectResourceArgs
  { js_wrapper =
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  window.addEventListener('" <> RawJavaScript eventName <> "', listener);\n\
      \  return () => window.removeEventListener('" <> RawJavaScript eventName <> "', listener);\n\
      \})"
  , mk_callback = \k event -> liftIO (fromJSVal event) >>= mapM_ k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardConnectArgs :: Text -> ConnectResourceArgs (Int -> ClickM ())
keyboardConnectArgs eventName = ConnectResourceArgs
  { js_wrapper =
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  window.addEventListener('" <> RawJavaScript eventName <> "', listener);\n\
      \  return () => window.removeEventListener('" <> RawJavaScript eventName <> "', listener);\n\
      \})"
  , mk_callback = \k event -> liftIO (fromJSVal event) >>= mapM_ k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
focusConnectArgs :: Text -> ConnectResourceArgs (ClickM ())
focusConnectArgs eventName = ConnectResourceArgs
  { js_wrapper = normalEventWrapper eventName defaultEventListenerOptions
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeConnectArgs :: ConnectResourceArgs (Bool -> ClickM ())
checkboxChangeConnectArgs = ConnectResourceArgs
  { js_wrapper =
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.checked);\n\
      \  }\n\
      \  window.addEventListener('change', listener);\n\
      \  return () => window.removeEventListener('change', listener);\n\
      \})"
  , mk_callback = \k event -> liftIO (fromJSVal event) >>= mapM_ k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeConnectArgs :: ConnectResourceArgs (Text -> ClickM ())
selectChangeConnectArgs = ConnectResourceArgs
  { js_wrapper =
      "(function(target, haskellCb){\n\
      \  function listener(target){\n\
      \    haskellCb(event.target.value);\n\
      \  }\n\
      \  window.addEventListener('change', listener);\n\
      \  return () => window.removeEventListener('change', listener);\n\
      \})"
  , mk_callback = \k event -> liftIO (fromJSVal event) >>= mapM_ k
  }

normalEventWrapper :: Text -> EventListenerOptions -> RawJavaScript
normalEventWrapper eventName opt =
  "(function(target, haskellCb){\n\
  \  function listener(event){\n\
  \    " <> preventDefaultStmt <> ";\n\
  \    " <> stopPropagationStmt <> ";\n\
  \    haskellCb(event);\n\
  \  }\n\
  \  target.addEventListener('" <> RawJavaScript eventName <> "', listener);\n\
  \  return () => target.removeEventListener(" <> RawJavaScript eventName <> ", listener);\n\
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
    deriving anyclass (FromJSVal, ToJSVal)

-- https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event
popstateConnectArgs :: ConnectResourceArgs (Location -> ClickM ())
popstateConnectArgs = ConnectResourceArgs
  { js_wrapper =
      "(function(target, haskellCb){\n\
      \  function listener(){\n\
      \    haskellCb(location);\n\
      \  }\n\
      \  target.addEventListener('popstate', listener);\n\
      \  return () => target.removeEventListener('popstate', listener);\n\
      \})"
  , mk_callback = \k event -> liftIO (fromJSVal event) >>= mapM_ k
  }
