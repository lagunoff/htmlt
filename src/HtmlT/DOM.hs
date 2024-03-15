{-|
Functions and definitions to manipulate and query the DOM tree
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE JavaScriptFFI #-}
#endif
module HtmlT.DOM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Coerce
import GHC.Exts as Exts
import GHC.Generics hiding (R)
import Unsafe.Coerce
import Wasm.Compat.Prim
import Wasm.Compat.Marshal
import HtmlT.Types
import HtmlT.Event
import Data.Text

import Data.Kind

data EventListenerOptions = EventListenerOptions
  { prevent_default :: Bool
  , stop_propagation :: Bool
  } deriving stock (Generic, Show, Eq)

defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions
  { prevent_default = False
  , stop_propagation = False
  }

on :: forall eventName. IsEventName eventName => EventListenerCb eventName -> Html ()
on k = addEventListener (addEventListenerArgs @eventName) k

data AddEventListenerArgs callback = AddEventListenerArgs
  { event_name :: Text
  , listener_options :: EventListenerOptions
  , mk_callback :: callback -> JSVal -> R ()
  } deriving (Generic)

addEventListener :: AddEventListenerArgs callback -> callback -> Html ()
addEventListener args k = do
  el <- asks (.html_current_element)
  addEventListenerTarget el.unDOMElement args k

-- | EventTarget.addEventListener()
-- https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
addEventListenerTarget
  :: MonadIO m
  => JSVal
  -> AddEventListenerArgs callback
  -> callback
  -> m ()
addEventListenerTarget target args k = do
  cb <- liftIO $ js_dynExport1 $ trampoline . args.mk_callback k
  jEventName <- liftIO $ toJSVal args.event_name
  -- jscb <- withopts hscb
  liftIO $ js_addEventListener target jEventName cb
  let reactiveScope = 1::Int
      installFinalizer _ _ = return ()
  installFinalizer reactiveScope do
    js_addEventListener target jEventName cb
    freeJSVal cb

class IsEventName eventName where
  type EventListenerCb eventName :: Type
  addEventListenerArgs :: AddEventListenerArgs (EventListenerCb eventName)

instance IsEventName "click" where
  type EventListenerCb "click" = R ()
  addEventListenerArgs = pointerEventArgs "click"

instance IsEventName "mousedown" where
  type EventListenerCb "mousedown" = R ()
  addEventListenerArgs = pointerEventArgs "mousedown"

instance IsEventName "mouseup" where
  type EventListenerCb "mouseup" = R ()
  addEventListenerArgs = pointerEventArgs "mouseup"

instance IsEventName "dblclick" where
  type EventListenerCb "dblclick" = R ()
  addEventListenerArgs = pointerEventArgs "dblclick"

instance IsEventName "submit" where
  type EventListenerCb "submit" = R ()
  addEventListenerArgs = submitEventArgs

instance IsEventName "input" where
  type EventListenerCb "input" = Text -> R ()
  addEventListenerArgs = inputEventArgs

instance IsEventName "keydown" where
  type EventListenerCb "keydown" = Int -> R ()
  addEventListenerArgs = keyboardEventArgs "keydown"

instance IsEventName "keyup" where
  type EventListenerCb "keyup" = Int -> R ()
  addEventListenerArgs = keyboardEventArgs "keyup"

instance IsEventName "focus" where
  type EventListenerCb "focus" = R ()
  addEventListenerArgs = pointerEventArgs "focus"

instance IsEventName "blur" where
  type EventListenerCb "blur" = R ()
  addEventListenerArgs = pointerEventArgs "blur"

instance IsEventName "input/blur" where
  type EventListenerCb "input/blur" = Text -> R ()
  addEventListenerArgs = inputEventArgs {event_name = "blur"}

instance IsEventName "input/focus" where
  type EventListenerCb "input/focus" = Text -> R ()
  addEventListenerArgs = inputEventArgs {event_name = "focus"}

instance IsEventName "checkbox/change" where
  type EventListenerCb "checkbox/change" = Bool -> R ()
  addEventListenerArgs = checkboxChangeEventArgs

instance IsEventName "select/change" where
  type EventListenerCb "select/change" = Text -> R ()
  addEventListenerArgs = selectChangeEventArgs

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
pointerEventArgs :: Text -> AddEventListenerArgs (R ())
pointerEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event
submitEventArgs :: AddEventListenerArgs (R ())
submitEventArgs = AddEventListenerArgs
  { event_name = "submit"
  , listener_options = defaultSubmitOptions
  , mk_callback = \k _ -> k
  }
  where
    defaultSubmitOptions = EventListenerOptions True True

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputEventArgs :: AddEventListenerArgs (Text -> R ())
inputEventArgs = AddEventListenerArgs
  { event_name = "input"
  , listener_options = defaultEventListenerOptions
  , mk_callback = \k event -> do
    target <- liftIO $ js_getProp event "target"
    value <- liftIO $ js_getProp target "value"
    liftIO (fromJSVal value) >>= mapM_ k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardEventArgs :: Text -> AddEventListenerArgs (Int -> R ())
keyboardEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_callback = \k event -> do
    keyCode <- liftIO $ js_getProp event "keyCode"
    liftIO (fromJSVal keyCode) >>= mapM_ k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
focusEventArgs :: Text -> AddEventListenerArgs (R ())
focusEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_callback = \k _ -> k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeEventArgs :: AddEventListenerArgs (Bool -> R ())
checkboxChangeEventArgs = AddEventListenerArgs
  { event_name = "change"
  , listener_options = defaultEventListenerOptions
  , mk_callback = \k event -> do
    target <- liftIO $ js_getProp event "target"
    checked <- liftIO $ js_getProp target "checked"
    liftIO (fromJSVal checked) >>= mapM_ k
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeEventArgs :: AddEventListenerArgs (Text -> R ())
selectChangeEventArgs = AddEventListenerArgs
  { event_name = "change"
  , listener_options = defaultEventListenerOptions
  , mk_callback = \k event -> do
    target <- liftIO $ js_getProp event "target"
    value <- liftIO $ js_getProp target "value"
    liftIO (fromJSVal value) >>= mapM_ k
  }

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

-- https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event
popstateEventArgs :: AddEventListenerArgs (Location -> R ())
popstateEventArgs = AddEventListenerArgs
  { event_name = "popstate"
  , listener_options = defaultEventListenerOptions
  , mk_callback =  \k _event -> do
    win <- liftIO js_getCurrentWindow
    loc <- liftIO $ js_getProp win "location"
    mprotocol <- liftIO $ fromJSVal =<< js_getProp loc "protocol"
    mhostname <- liftIO $ fromJSVal =<< js_getProp loc "hostname"
    mport <- liftIO $ fromJSVal =<< js_getProp loc "port"
    mpathname <- liftIO $ fromJSVal =<< js_getProp loc "pathname"
    msearch <- liftIO $ fromJSVal =<< js_getProp loc "search"
    mhash <- liftIO $ fromJSVal =<< js_getProp loc "hash"
    mapM_ k do
      protocol <- mprotocol
      hostname <- mhostname
      port <- mport
      pathname <- mpathname
      search <- msearch
      hash <- mhash
      return Location {..}
  }

-- | Get global Window object @window@
-- https://developer.mozilla.org/en-US/docs/Web/API/Window
getCurrentWindow :: MonadIO m => m JSVal
getCurrentWindow = liftIO js_getCurrentWindow

-- | Get global Document object
-- https://developer.mozilla.org/en-US/docs/Web/API/Document
getCurrentDocument :: MonadIO m => m JSVal
getCurrentDocument = liftIO js_getCurrentDocument

-- | Get Document.body property
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/body
getCurrentBody :: MonadIO m => m DOMElement
getCurrentBody = liftIO $ fmap DOMElement js_getCurrentBody

-- | DOMElement.appendChild()
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/appendChild
appendChild :: DOMElement -> DOMNode -> IO ()
appendChild = js_appendChild

-- | Element.setAttribute()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute
setAttribute :: DOMElement -> Text -> Text -> IO ()
setAttribute e k v = do
  jk <- textToJSString k
  jv <- textToJSString v
  js_setAttribute e jk jv

-- | Element.removeAttribute()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttribute
removeAttribute :: DOMElement -> Text -> IO ()
removeAttribute e k = do
  jk <- textToJSString k
  js_removeAttribute e jk

-- | DOMNode.removeChild()
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/removeChild
removeChild :: DOMElement -> DOMNode -> IO ()
removeChild = js_removeChild

-- | Document.createElement()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement
createElement :: Text -> IO DOMElement
createElement tagName = do
  jTagName <- textToJSString tagName
  js_createElement jTagName

-- | Document.createElementNS()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS
createElementNS :: Text -> Text -> IO DOMElement
createElementNS ns tagName = do
  jNs <- textToJSString ns
  jTagName <- textToJSString tagName
  js_createElementNS jNs jTagName

-- | Document.createTextNode()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode
createTextNode :: Text -> IO DOMNode
createTextNode t = do
  jt <- textToJSString t
  js_createTextNode jt

-- | Document.createComment()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createComment
createComment :: Text -> IO DOMNode
createComment c = do
  jc <- textToJSString c
  js_createComment jc

-- | Element.classList.add()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
classListAdd :: DOMElement -> Text -> IO ()
classListAdd e c = do
  jc <- textToJSString c
  js_classListAdd e jc

-- | Element.classList.remove()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
classListRemove :: DOMElement -> Text -> IO ()
classListRemove e c = do
  jc <- textToJSString c
  js_classListRemove e jc

-- | Assign text to DOMNode.nodeValue
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/nodeValue
setTextValue :: DOMNode -> Text -> IO ()
setTextValue n v = do
  jv <- textToJSString v
  js_setTextValue n jv

-- | Insert raw HTML code, similar to @parent.innerHTML = rawHtml@ but
-- does not removes siblings
unsafeInsertHtml :: DOMElement -> Maybe DOMNode -> Text -> IO ()
unsafeInsertHtml parent manchor rawHtml = do
  jRawHtml <- textToJSString rawHtml
  js_unsafeInsertHtml parent (maybeToNullable manchor) jRawHtml

-- | Assuming given 'ContentBoundary' was inserted into the @parent@
-- element remove all the content inside the boundary.
clearBoundary :: ContentBoundary -> IO ()
clearBoundary b =
  js_clearBoundary b.boundary_begin b.boundary_end

-- | Detach 'ContentBoundary' from the DOM and everything inside the
-- boundary.
removeBoundary :: ContentBoundary -> IO ()
removeBoundary b = do
  js_clearBoundary b.boundary_begin b.boundary_end
  js_detachBoundary b.boundary_begin b.boundary_end

-- | Run a given callback on BeforeUnloadEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/BeforeUnloadEvent
onBeforeUnload :: IO () -> IO ()
onBeforeUnload cb = do
  jscb <- js_dynExport1 (const cb)
  js_onBeforeUnload jscb
  return ()

-- | Collection of deltaX, deltaY and deltaZ properties from WheelEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
data MouseDelta = MouseDelta
  { md_delta_x :: Int
  , md_delta_y :: Int
  , md_delta_z :: Int
  } deriving stock (Eq, Show, Generic)

-- mouseDeltaDecoder :: MonadIO m => JSVal -> MaybeT m MouseDelta
-- mouseDeltaDecoder mouseEvent = do
--   md_delta_x <- propDecoder "deltaX" mouseEvent
--   md_delta_y <- propDecoder "deltaY" mouseEvent
--   md_delta_z <- propDecoder "deltaZ" mouseEvent
--   return MouseDelta {..}

-- | Pair of two values, might denote either a size or coordinates in
-- different contexts
data Point a = Point
  { pt_x :: a
  , pt_y :: a
  } deriving stock (Eq, Show, Ord, Functor, Generic)

-- -- | Read clientX and clientY properties from MouseEvent
-- -- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
-- clientXYDecoder :: MonadIO m => JSVal -> MaybeT m (Point Int)
-- clientXYDecoder mouseEvent = do
--   pt_x <- propDecoder "clientX" mouseEvent
--   pt_y <- propDecoder "clientY" mouseEvent
--   return Point {..}

-- -- | Read offsetX and offsetY properties from MouseEvent
-- -- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
-- offsetXYDecoder :: MonadIO m => JSVal -> MaybeT m (Point Int)
-- offsetXYDecoder mouseEvent = do
--   pt_x <- propDecoder "offsetX" mouseEvent
--   pt_y <- propDecoder "offsetY" mouseEvent
--   return Point {..}

-- -- | Read pageX and pageY properties from MouseEvent
-- -- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
-- pageXYDecoder :: MonadIO m => JSVal -> MaybeT m (Point Int)
-- pageXYDecoder mouseEvent = do
--   pt_x <- propDecoder "pageX" mouseEvent
--   pt_y <- propDecoder "pageY" mouseEvent
--   return Point {..}

-- -- | Collection of altKey, ctrlKey, metaKey and shiftKey properties
-- -- from KeyboardEvent
-- data KeyModifiers = KeyModifiers
--   { kmod_alt_key :: Bool
--   , kmod_ctrl_key :: Bool
--   , kmod_meta_key :: Bool
--   , kmod_shift_key :: Bool
--   } deriving stock (Eq, Show, Generic)

-- -- | Read altKey, ctrlKey, metaKey and shiftKey properties from
-- -- KeyboardEvent
-- -- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
-- keyModifiersDecoder :: MonadIO m => JSVal -> MaybeT m KeyModifiers
-- keyModifiersDecoder keyEvent = do
--   kmod_alt_key <- propDecoder "altKey" keyEvent
--   kmod_ctrl_key <- propDecoder "ctrlKey" keyEvent
--   kmod_meta_key <- propDecoder "metaKey" keyEvent
--   kmod_shift_key <- propDecoder "shiftKey" keyEvent
--   return KeyModifiers {..}

-- -- | Read keyCode properties from KeyboardEvent
-- -- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode
-- keyCodeDecoder :: MonadIO m => JSVal -> MaybeT m Int
-- keyCodeDecoder = propDecoder "keyCode"

-- -- | Collection of some useful information from KeyboardEvent
-- data KeyboardEvent = KeyboardEvent
--   { ke_modifiers :: KeyModifiers
--   , ke_key :: Maybe JSString
--   , ke_key_code :: Int
--   , ke_repeat :: Bool
--   } deriving stock (Generic)

-- -- | Read information from KeyboardEvent
-- keyboardEventDecoder :: MonadIO m => JSVal -> MaybeT m KeyboardEvent
-- keyboardEventDecoder keyEvent = do
--   ke_modifiers <- keyModifiersDecoder keyEvent
--   ke_key <- propDecoder "key" keyEvent
--   ke_key_code <- propDecoder "keyCode" keyEvent
--   ke_repeat <- propDecoder "repeat" keyEvent
--   return KeyboardEvent {..}

errorGhcjsOnly :: a
errorGhcjsOnly = error "Only GHCJS is supported"

#if !defined(wasm32_HOST_ARCH)
js_onBeforeUnload :: JSVal {-Callback a-} -> IO ()
js_onBeforeUnload = errorGhcjsOnly

js_appendChild :: DOMElement -> DOMNode -> IO () = errorGhcjsOnly
js_insertBefore :: DOMElement -> DOMNode -> DOMNode -> IO () = errorGhcjsOnly
js_clearBoundary :: DOMNode -> DOMNode -> IO () = errorGhcjsOnly
js_detachBoundary :: DOMNode -> DOMNode -> IO () = errorGhcjsOnly
js_setAttribute :: DOMElement -> JSString -> JSString -> IO () = errorGhcjsOnly
js_removeAttribute :: DOMElement -> JSString -> IO ()  = errorGhcjsOnly
js_removeChild :: DOMElement -> DOMNode -> IO ()  = errorGhcjsOnly
js_replaceChild :: DOMElement -> DOMNode -> DOMNode -> IO ()  = errorGhcjsOnly
js_createElement :: JSString -> IO DOMElement  = errorGhcjsOnly
js_createElementNS :: JSString -> JSString -> IO DOMElement  = errorGhcjsOnly
js_createTextNode :: JSString -> IO DOMNode  = errorGhcjsOnly
js_createComment :: JSString -> IO DOMNode  = errorGhcjsOnly
js_classListAdd :: DOMElement -> JSString -> IO ()  = errorGhcjsOnly
js_classListRemove :: DOMElement -> JSString -> IO ()  = errorGhcjsOnly
js_setTextValue :: DOMNode -> JSString -> IO ()  = errorGhcjsOnly
js_getCurrentWindow :: IO JSVal  = errorGhcjsOnly
js_getCurrentDocument :: IO JSVal  = errorGhcjsOnly
js_getCurrentBody :: IO JSVal = errorGhcjsOnly
js_unsafeInsertHtml :: DOMElement -> Nullable DOMNode -> JSString -> IO () = errorGhcjsOnly
js_waitDocumentLoad :: IO () = errorGhcjsOnly
js_callbackWithOptions :: Bool -> Bool -> JSVal {-Callback (JSVal -> IO ())-} -> IO JSVal {-(Callback (JSVal -> IO ()))-} = errorGhcjsOnly
js_setProp :: JSVal -> JSString -> JSVal -> IO () = errorGhcjsOnly
js_dynExport1 :: (JSVal -> IO ()) -> IO JSVal = errorGhcjsOnly
js_addEventListener :: JSVal -> JSVal -> JSVal -> IO () = errorGhcjsOnly
js_removeEventListener :: JSVal -> JSVal -> JSVal -> IO () = errorGhcjsOnly

#else
foreign import javascript unsafe
  "$1.appendChild($2)"
  js_appendChild :: DOMElement -> DOMNode -> IO ()
foreign import javascript unsafe
  "$1.insertBefore($2, $3)"
  js_insertBefore :: DOMElement -> DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  js_setAttribute :: DOMElement -> JSString -> JSString -> IO ()
foreign import javascript unsafe
  "$1.removeAttribute($2)"
  js_removeAttribute :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "$1.removeChild($2)"
  js_removeChild :: DOMElement -> DOMNode -> IO ()
foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  js_replaceChild :: DOMElement -> DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "document.createElement($1)"
  js_createElement :: JSString -> IO DOMElement
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  js_createElementNS :: JSString -> JSString -> IO DOMElement
foreign import javascript unsafe
  "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO DOMNode
foreign import javascript unsafe
  "document.createComment($1)"
  js_createComment :: JSString -> IO DOMNode
foreign import javascript unsafe
  "$1.classList.add($2);"
  js_classListAdd :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "$1.classList.remove($2);"
  js_classListRemove :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "$1.nodeValue = $2;"
  js_setTextValue :: DOMNode -> JSString -> IO ()
foreign import javascript safe
  "window.addEventListener('beforeunload', $1)"
  js_onBeforeUnload :: JSVal -> IO ()
foreign import javascript unsafe
  "return window"
  js_getCurrentWindow :: IO JSVal
foreign import javascript unsafe
  "window.document"
  js_getCurrentDocument :: IO JSVal
foreign import javascript unsafe
  "window.document.body"
  js_getCurrentBody :: IO JSVal
foreign import javascript unsafe
  "for (;;){\
    if (!$2.previousSibling\
      || !$2.previousSibling.parentNode\
      || $2.previousSibling === $1\
      ) break;\
    $2.previousSibling.parentNode.removeChild($2.previousSibling);\
  }"
  js_clearBoundary :: DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "if ($1.parentNode) $1.parentNode.removeChild($1);\
   if ($2.parentNode) $2.parentNode.removeChild($2);"
  js_detachBoundary :: DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "div.innerHTML = $3;\
   var tempChilds = [];\
   for (var i = 0; i < div.childNodes.length; i++) {\
     tempChilds.push(div.childNodes[i]);\
   }\
   for (var j = 0; j < tempChilds.length; j++) {\
     div.removeChild(tempChilds[j]);\
     if ($2) {\
       $1.insertBefore(tempChilds[j], $2);\
     } else{\
       $1.appendChild(tempChilds[j]);\
     }\
   }"
  js_unsafeInsertHtml :: DOMElement -> Nullable DOMNode -> JSString -> IO ()
foreign import javascript interruptible
  "if (document.readyState == 'loading') {\
    addEventListener('DOMContentLoaded', $c);\
  } else {\
    $c();\
  }"
  js_waitDocumentLoad :: IO ()
foreign import javascript unsafe
  "$1[$2] = $3;"
  js_setProp :: JSVal -> JSString -> JSVal -> IO ()
foreign import javascript "wrapper"
  js_dynExport1 :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript safe
  "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript safe
  "$1.removeEventListener($2, $3)"
  js_removeEventListener :: JSVal -> JSVal -> JSVal -> IO ()
#endif

instance (a ~ (), MonadIO m) => IsString (HtmlT m a) where
  fromString s = do
    HtmlEnv{html_current_element, html_content_boundary} <- ask
    let jsstr = toJSString s
    textNode <- liftIO $ js_createTextNode jsstr
    case html_content_boundary of
      Just b -> liftIO $
        js_insertBefore html_current_element textNode b.boundary_end
      Nothing -> liftIO $ appendChild html_current_element textNode
  {-# INLINE fromString #-}
