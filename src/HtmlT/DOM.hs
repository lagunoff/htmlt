-- | Functions and definitions to work with DOM. This exists because
-- ghcjs-dom is too heavy, takes about an hour to compile and
-- increases size of the generated JavaScript
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module HtmlT.DOM where

import Control.Monad.Reader
import Data.Coerce
import Data.Text as T
import Data.JSString.Text
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Prim as Prim
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Nullable

import HtmlT.Decode
import HtmlT.Types

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
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSVal)

defaultListenerOpts :: ListenerOpts
defaultListenerOpts = ListenerOpts True False False

-- | Get global Window object @window@
-- https://developer.mozilla.org/en-US/docs/Web/API/Window
getCurrentWindow :: MonadIO m => m JSVal
getCurrentWindow = liftIO js_getCurrentWindow

-- | Get global Window object
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
setAttribute e k v = js_setAttribute e (textToJSString k) (textToJSString v)

-- | Element.removeAttribute()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttribute
removeAttribute :: DOMElement -> Text -> IO ()
removeAttribute e k = js_removeAttribute e (textToJSString k)

-- | DOMNode.removeChild()
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/removeChild
removeChild :: DOMElement -> DOMNode -> IO ()
removeChild = js_removeChild

-- | Document.createElement()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement
createElement :: Text -> IO DOMElement
createElement = js_createElement . textToJSString

-- | Document.createElementNS()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS
createElementNS :: Text -> Text -> IO DOMElement
createElementNS n t = js_createElementNS (textToJSString n) (textToJSString t)

-- | Document.createTextNode()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode
createTextNode :: Text -> IO DOMNode
createTextNode = js_createTextNode . textToJSString

-- | Document.createComment()
-- https://developer.mozilla.org/en-US/docs/Web/API/Document/createComment
createComment :: Text -> IO DOMNode
createComment = js_createComment . textToJSString

-- | Element.classList.add()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
classListAdd :: DOMElement -> Text -> IO ()
classListAdd e c = js_classListAdd e (textToJSString c)

-- | Element.classList.remove()
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/classList
classListRemove :: DOMElement -> Text -> IO ()
classListRemove e c = js_classListRemove e (textToJSString c)

-- | Assign text to DOMNode.nodeValue
-- https://developer.mozilla.org/en-US/docs/Web/API/DOMNode/nodeValue
setTextValue :: DOMNode -> Text -> IO ()
setTextValue v = js_setTextValue v . textToJSString

-- | Insert raw HTML code, similar to @parent.innerHTML = rawHtml@ but
-- does not removes siblings
unsafeInsertHtml :: DOMElement -> Maybe DOMNode -> Text -> IO ()
unsafeInsertHtml parent manchor rawHtml = js_unsafeInsertHtml parent
  (maybeToNullable manchor) (textToJSString rawHtml)

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
    (jsval (textToJSString name)) (jsval jscb)
  return do
    js_callMethod2 (coerce target) "removeEventListener"
      (jsval (textToJSString name)) (jsval jscb)
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

mouseDeltaDecoder :: Decoder MouseDelta
mouseDeltaDecoder = MouseDelta
  <$> decodeAt ["deltaX"] decoder
  <*> decodeAt ["deltaY"] decoder
  <*> decodeAt ["deltaZ"] decoder

-- | Collection of @X@ and @Y@ coordinates, intended to extract
-- position from MouseEvent
data Position = Position
  { pos_x :: Int
  , pos_y :: Int
  } deriving stock (Eq, Show, Ord, Generic)

-- | Read clientX and clientY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
clientXYDecoder :: Decoder Position
clientXYDecoder = Position
  <$> decodeAt ["clientX"] decoder
  <*> decodeAt ["clientY"] decoder

-- | Read offsetX and offsetY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
offsetXYDecoder :: Decoder Position
offsetXYDecoder = Position
  <$> decodeAt ["offsetX"] decoder
  <*> decodeAt ["offsetY"] decoder

-- | Read pageX and pageY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
pageXYDecoder :: Decoder Position
pageXYDecoder = Position
  <$> decodeAt ["pageX"] decoder
  <*> decodeAt ["pageY"] decoder

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
keyModifiersDecoder :: Decoder KeyModifiers
keyModifiersDecoder = KeyModifiers
  <$> decodeAt ["altKey"] decoder
  <*> decodeAt ["ctrlKey"] decoder
  <*> decodeAt ["metaKey"] decoder
  <*> decodeAt ["shiftKey"] decoder

-- | Read keyCode properties from KeyboardEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode
keyCodeDecoder :: Decoder Int
keyCodeDecoder = decodeAt ["keyCode"] decoder

-- | Collection of some useful information from KeyboardEvent
data KeyboardEvent = KeyboardEvent
  { ke_modifiers :: KeyModifiers
  , ke_key :: Maybe Text
  , ke_key_code :: Int
  , ke_repeat :: Bool
  } deriving stock (Eq, Show, Generic)

-- | Read information from KeyboardEvent
keyboardEventDecoder :: Decoder KeyboardEvent
keyboardEventDecoder = KeyboardEvent
  <$> keyModifiersDecoder
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder

-- | Event.target
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
targetDecoder :: Decoder JSVal
targetDecoder = decodeAt ["target"] decodeJSVal

-- | Event.currentTarget
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/currentTarget
currentTargetDecoder :: Decoder JSVal
currentTargetDecoder = decodeAt ["currentTarget"] decodeJSVal

-- | Event.target.value
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#attr-value
valueDecoder :: Decoder Text
valueDecoder = decodeAt ["target", "value"] decoder

-- | Event.target.value
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/checkbox#checked
checkedDecoder :: Decoder Bool
checkedDecoder = decodeAt ["target", "checked"] decoder

errorGhcjsOnly :: a
errorGhcjsOnly = error "Only GHCJS is supported"

#ifndef ghcjs_HOST_OS
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
js_callbackWithOptions :: Bool -> Bool -> Callback (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ())) = errorGhcjsOnly
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
  "$1.classList.add($2)"
  js_classListAdd :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "$1.classList.remove($2)"
  js_classListRemove :: DOMElement -> JSString -> IO ()
foreign import javascript unsafe
  "$1.nodeValue = $2;"
  js_setTextValue :: DOMNode -> JSString -> IO ()
foreign import javascript unsafe
  "window.addEventListener('beforeunload', function() { $1(); })"
  js_onBeforeUnload :: Callback a -> IO ()
foreign import javascript unsafe
  "(function(){ return window; })()"
  js_getCurrentWindow :: IO JSVal
foreign import javascript unsafe
  "(function(){ return window.document; })()"
  js_getCurrentDocument :: IO JSVal
foreign import javascript unsafe
  "(function(){ return window.document.body; })()"
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
  })($1, $2)"
  js_clearBoundary :: DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe
  "(function (begin, end) {\
    if (begin.parentNode) begin.parentNode.removeChild(begin);\
    if (end.parentNode) end.parentNode.removeChild(end);\
  })($1, $2)"
  js_detachBoundary :: DOMNode -> DOMNode -> IO ()
foreign import javascript unsafe "$1()"
  js_call0 :: JSVal -> IO JSVal
foreign import javascript unsafe "$1($2)"
  js_call1 :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1($2, $3)"
  js_call2 :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1[$2]()"
  js_callMethod0 :: JSVal -> JSString -> IO JSVal
foreign import javascript unsafe "$1[$2]($3)"
  js_callMethod1 :: JSVal -> JSString -> JSVal -> IO JSVal
foreign import javascript unsafe "$1[$2]($3, $4)"
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
  })($1, $2, $3)"
  js_unsafeInsertHtml :: DOMElement -> Nullable DOMNode -> JSString -> IO ()
foreign import javascript unsafe
  "$r = function(e) {\
    if ($1) e.stopPropagation();\
    if ($2) e.preventDefault();\
    return $3(e);\
  }"
  js_callbackWithOptions :: Bool -> Bool -> Callback (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
#endif
