-- | Functions and definitions to work with DOM. This exists because
-- ghcjs-dom is too heavy, takes about an hour to compile and
-- increases size of the generated JavaScript
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module HtmlT.DOM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Coerce
import GHC.Exts as Exts
import GHC.Generics
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Unsafe.Coerce

import HtmlT.Types
import JavaScript.Compat.Marshal
import JavaScript.Compat.String (JSString(..))

data ListenerOpts = ListenerOpts
  { lo_stop_propagation :: Bool
  -- ^ If true call @event.stopPropagation()@
  , lo_prevent_default :: Bool
  -- ^ If true call @event.preventDefault()@
  , lo_sync_callback :: Bool
  -- ^ If true create callback with @syncCallback1 ThrowWouldBlock@
  -- otherwise — @asyncCallback1@ this is relevant for example when
  -- listening to @BeforeUnloadEvent@
  -- https://developer.mozilla.org/en-US/docs/Web/API/BeforeUnloadEvent
  } deriving stock (Generic)

defaultListenerOpts :: ListenerOpts
defaultListenerOpts = ListenerOpts False False False

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
setAttribute :: DOMElement -> JSString -> JSString -> IO ()
setAttribute e k v = js_setAttribute e k v

-- | Element.removeAttribute()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttribute
removeAttribute :: DOMElement -> JSString -> IO ()
removeAttribute e k = js_removeAttribute e k

-- | DOMNode.removeChild()
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/removeChild
removeChild :: DOMElement -> DOMNode -> IO ()
removeChild = js_removeChild

-- | Document.createElement()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement
createElement :: JSString -> IO DOMElement
createElement = js_createElement

-- | Document.createElementNS()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS
createElementNS :: JSString -> JSString -> IO DOMElement
createElementNS n t = js_createElementNS n t

-- | Document.createTextNode()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode
createTextNode :: JSString -> IO DOMNode
createTextNode = js_createTextNode

-- | Document.createComment()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createComment
createComment :: JSString -> IO DOMNode
createComment = js_createComment

-- | Element.classList.add()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
classListAdd :: DOMElement -> JSString -> IO ()
classListAdd e c = js_classListAdd e c

-- | Element.classList.remove()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
classListRemove :: DOMElement -> JSString -> IO ()
classListRemove e c = js_classListRemove e c

-- | Assign text to DOMNode.nodeValue
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/nodeValue
setTextValue :: DOMNode -> JSString -> IO ()
setTextValue v = js_setTextValue v

-- | Insert raw HTML code, similar to @parent.innerHTML = rawHtml@ but
-- does not removes siblings
unsafeInsertHtml :: DOMElement -> Maybe DOMNode -> JSString -> IO ()
unsafeInsertHtml parent manchor rawHtml = js_unsafeInsertHtml parent
  (nullableFromMaybe manchor) rawHtml

-- | Assuming given 'ContentBoundary' was inserted into the @parent@
-- element remove all the content inside the boundary.
clearBoundary :: ContentBoundary -> IO ()
clearBoundary ContentBoundary{..} =
  js_clearBoundary boundary_begin boundary_end

-- | Detach 'ContentBoundary' from the DOM and everything inside the
-- boundary.
removeBoundary :: ContentBoundary -> IO ()
removeBoundary ContentBoundary{..} = do
  js_clearBoundary boundary_begin boundary_end
  js_detachBoundary boundary_begin boundary_end

-- | Run a given callback on BeforeUnloadEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/BeforeUnloadEvent
onBeforeUnload :: IO () -> IO ()
onBeforeUnload cb = do
  syncCb <- syncCallback ThrowWouldBlock cb
  js_onBeforeUnload syncCb

-- | EventTarget.addEventListener()
-- https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
addEventListener
  :: ListenerOpts
  -> DOMNode
  -> EventName
  -> (DOMEvent -> IO ())
  -> IO (IO ())
addEventListener ListenerOpts{..} target name f = do
  hscb <- mkcallback (f . DOMEvent)
  jscb <- withopts hscb
  js_callMethod2 (coerce target) "addEventListener"
    (unJSString (unEventName name)) (unsafeCoerce jscb)
  return do
    js_callMethod2 (coerce target) "removeEventListener"
      (unJSString (unEventName name)) (unsafeCoerce jscb)
    releaseCallback hscb
  where
    mkcallback = if lo_sync_callback
      then syncCallback1 ThrowWouldBlock
      else asyncCallback1
    withopts = js_callbackWithOptions lo_stop_propagation lo_prevent_default

-- | Collection of deltaX, deltaY and deltaZ properties from WheelEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
data MouseDelta = MouseDelta
  { md_delta_x :: Int
  , md_delta_y :: Int
  , md_delta_z :: Int
  } deriving stock (Eq, Show, Generic)

mouseDeltaDecoder :: MonadIO m => JSVal -> MaybeT m MouseDelta
mouseDeltaDecoder mouseEvent = do
  md_delta_x <- propDecoder "deltaX" mouseEvent
  md_delta_y <- propDecoder "deltaY" mouseEvent
  md_delta_z <- propDecoder "deltaZ" mouseEvent
  return MouseDelta {..}

-- | Pair of two values, might denote either a size or coordinates in
-- different contexts
data Point a = Point
  { pt_x :: a
  , pt_y :: a
  } deriving stock (Eq, Show, Ord, Functor, Generic)

-- | Read clientX and clientY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
clientXYDecoder :: MonadIO m => JSVal -> MaybeT m (Point Int)
clientXYDecoder mouseEvent = do
  pt_x <- propDecoder "clientX" mouseEvent
  pt_y <- propDecoder "clientY" mouseEvent
  return Point {..}

-- | Read offsetX and offsetY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
offsetXYDecoder :: MonadIO m => JSVal -> MaybeT m (Point Int)
offsetXYDecoder mouseEvent = do
  pt_x <- propDecoder "offsetX" mouseEvent
  pt_y <- propDecoder "offsetY" mouseEvent
  return Point {..}

-- | Read pageX and pageY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
pageXYDecoder :: MonadIO m => JSVal -> MaybeT m (Point Int)
pageXYDecoder mouseEvent = do
  pt_x <- propDecoder "pageX" mouseEvent
  pt_y <- propDecoder "pageY" mouseEvent
  return Point {..}

-- | Collection of altKey, ctrlKey, metaKey and shiftKey properties
-- from KeyboardEvent
data KeyModifiers = KeyModifiers
  { kmod_alt_key :: Bool
  , kmod_ctrl_key :: Bool
  , kmod_meta_key :: Bool
  , kmod_shift_key :: Bool
  } deriving stock (Eq, Show, Generic)

-- | Read altKey, ctrlKey, metaKey and shiftKey properties from
-- KeyboardEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
keyModifiersDecoder :: MonadIO m => JSVal -> MaybeT m KeyModifiers
keyModifiersDecoder keyEvent = do
  kmod_alt_key <- propDecoder "altKey" keyEvent
  kmod_ctrl_key <- propDecoder "ctrlKey" keyEvent
  kmod_meta_key <- propDecoder "metaKey" keyEvent
  kmod_shift_key <- propDecoder "shiftKey" keyEvent
  return KeyModifiers {..}

-- | Read keyCode properties from KeyboardEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode
keyCodeDecoder :: MonadIO m => JSVal -> MaybeT m Int
keyCodeDecoder = propDecoder "keyCode"

-- | Collection of some useful information from KeyboardEvent
data KeyboardEvent = KeyboardEvent
  { ke_modifiers :: KeyModifiers
  , ke_key :: Maybe JSString
  , ke_key_code :: Int
  , ke_repeat :: Bool
  } deriving stock (Generic)

-- | Read information from KeyboardEvent
keyboardEventDecoder :: MonadIO m => JSVal -> MaybeT m KeyboardEvent
keyboardEventDecoder keyEvent = do
  ke_modifiers <- keyModifiersDecoder keyEvent
  ke_key <- propDecoder "key" keyEvent
  ke_key_code <- propDecoder "keyCode" keyEvent
  ke_repeat <- propDecoder "repeat" keyEvent
  return KeyboardEvent {..}

-- | Event.target.value
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#attr-value
valueDecoder :: MonadIO m => JSVal -> MaybeT m JSString
valueDecoder =
  propDecoder "target" >=> propDecoder "value"

-- | Event.target.checked
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/checkbox#checked
checkedDecoder :: MonadIO m => JSVal -> MaybeT m Bool
checkedDecoder =
  propDecoder "target" >=> propDecoder "checked"

propDecoder :: (MonadIO m, FromJSVal v) => String -> JSVal -> MaybeT m v
propDecoder k obj = do
  -- TODO: Make sure it is true that if this guard succeeds,
  -- Object.getProp will never throw an exception!
  guard $ not (isUndefined obj) && not (isNull obj)
  MaybeT $ liftIO $ fromJSVal =<<
    getProp obj k

errorGhcjsOnly :: a
errorGhcjsOnly = error "Only GHCJS is supported"

#if !defined(javascript_HOST_ARCH)
js_onBeforeUnload :: Callback a -> IO ()
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
js_call0 :: JSVal -> IO JSVal = errorGhcjsOnly
js_call1 :: JSVal -> JSVal -> IO JSVal = errorGhcjsOnly
js_call2 :: JSVal -> JSVal -> JSVal -> IO JSVal = errorGhcjsOnly
js_callMethod0 :: JSVal -> JSString -> IO JSVal = errorGhcjsOnly
js_callMethod1 :: JSVal -> JSString -> JSVal -> IO JSVal = errorGhcjsOnly
js_callMethod2 :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal = errorGhcjsOnly
js_waitDocumentLoad :: IO () = errorGhcjsOnly
js_callbackWithOptions :: Bool -> Bool -> Callback (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ())) = errorGhcjsOnly
js_setProp :: JSVal -> JSVal -> JSVal -> IO () = errorGhcjsOnly
#else
foreign import javascript unsafe
  "(($1, $2) => $1.appendChild($2))"
  js_appendChild :: DOMElement -> DOMNode -> IO ()
foreign import javascript unsafe
  "(($1, $2, $3) => $1.insertBefore($2, $3))"
  js_insertBefore :: DOMElement -> DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "(($1, $2, $3) => $1.setAttribute($2, $3))"
  js_setAttribute :: DOMElement -> JSString -> JSString -> IO ()
foreign import javascript unsafe
  "(($1, $2) => $1.removeAttribute($2))"
  js_removeAttribute :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "(($1, $2) => $1.removeChild($2))"
  js_removeChild :: DOMElement -> DOMNode -> IO ()
foreign import javascript unsafe
  "(($1, $2, $3) => $1.replaceChild($2, $3))"
  js_replaceChild :: DOMElement -> DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "(($1) => document.createElement($1))"
  js_createElement :: JSString -> IO DOMElement
foreign import javascript unsafe
  "(($1, $2) => document.createElementNS($1, $2))"
  js_createElementNS :: JSString -> JSString -> IO DOMElement
foreign import javascript unsafe
  "(($1) => document.createTextNode($1))"
  js_createTextNode :: JSString -> IO DOMNode
foreign import javascript unsafe
  "(($1) => document.createComment($1))"
  js_createComment :: JSString -> IO DOMNode
foreign import javascript unsafe
  "(($1, $2) => $1.classList.add($2))"
  js_classListAdd :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "(($1, $2) => $1.classList.remove($2))"
  js_classListRemove :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "(($1, $2) => { $1.nodeValue = $2; })"
  js_setTextValue :: DOMNode -> JSString -> IO ()
foreign import javascript unsafe
  "(($1) => window.addEventListener('beforeunload', $1))"
  js_onBeforeUnload :: Callback a -> IO ()
foreign import javascript unsafe
  "(function(){ return window; })"
  js_getCurrentWindow :: IO JSVal
foreign import javascript unsafe
  "(function(){ return window.document; })"
  js_getCurrentDocument :: IO JSVal
foreign import javascript unsafe
  "(function(){ return window.document.body; })"
  js_getCurrentBody :: IO JSVal
foreign import javascript unsafe
  "(function (begin, end) {\
    for (;;){\
      if (!end.previousSibling\
        || !end.previousSibling.parentNode\
        || end.previousSibling === begin\
        ) break;\
      end.previousSibling.parentNode.removeChild(end.previousSibling);\
    }\
  })"
  js_clearBoundary :: DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "(function (begin, end) {\
    if (begin.parentNode) begin.parentNode.removeChild(begin);\
    if (end.parentNode) end.parentNode.removeChild(end);\
  })"
  js_detachBoundary :: DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe "(($1) => $1())"
  js_call0 :: JSVal -> IO JSVal
foreign import javascript unsafe "(($1, $2) => $1($2))"
  js_call1 :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "(($1, $2, $3) => $1($2, $3))"
  js_call2 :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "(($1, $2) => $1[$2]())"
  js_callMethod0 :: JSVal -> JSString -> IO JSVal
foreign import javascript unsafe "(($1, $2, $3) => $1[$2]($3))"
  js_callMethod1 :: JSVal -> JSString -> JSVal -> IO JSVal
foreign import javascript unsafe "(($1, $2, $3, $4) => $1[$2]($3, $4))"
  js_callMethod2 :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe
  "(function(el, anchor, htmlString){\
    var div = document.createElement('div');\
    div.innerHTML = htmlString;\
    var tempChilds = [];\
    for (var i = 0; i < div.childNodes.length; i++) {\
      tempChilds.push(div.childNodes[i]);\
    }\
    for (var j = 0; j < tempChilds.length; j++) {\
      div.removeChild(tempChilds[j]);\
      if (anchor) {\
        el.insertBefore(tempChilds[j], anchor);\
      } else{\
        el.appendChild(tempChilds[j]);\
      }\
    }\
  })"
  js_unsafeInsertHtml :: DOMElement -> Nullable DOMNode -> JSString -> IO ()
foreign import javascript unsafe
  "(($1, $2, $3) => function(e) {\
    if ($1) e.stopPropagation();\
    if ($2) e.preventDefault();\
    return $3(e);\
  })"
  js_callbackWithOptions :: Bool -> Bool -> Callback (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
foreign import javascript interruptible
  "if (document.readyState == 'loading') {\
    addEventListener('DOMContentLoaded', $c);\
  } else {\
    $c();\
  }"
  js_waitDocumentLoad :: IO ()
foreign import javascript unsafe
  "(($1, $2, $3) => { $1[$2] = $3; })"
  js_setProp :: JSVal -> JSString -> JSVal -> IO ()
foreign import javascript unsafe "(() => null)"
  js_null :: JSVal
#endif

instance (a ~ (), MonadIO m) => IsString (HtmlT m a) where
  fromString s = do
    HtmlEnv{html_current_element, html_content_boundary} <- ask
    let jsstr = toJSString s
    textNode <- liftIO $ createTextNode (JSString jsstr)
    case html_content_boundary of
      Just ContentBoundary{boundary_end} -> liftIO $
        js_insertBefore html_current_element textNode boundary_end
      Nothing -> liftIO $ appendChild html_current_element textNode
  {-# INLINE fromString #-}
