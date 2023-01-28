-- | Functions and definitions to work with DOM. This exists because
-- ghcjs-dom is too heavy, takes about an hour to compile and
-- increases size of the generated JavaScript
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module HtmlT.DOM where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.String
import Data.Text as T
import Data.JSString.Text
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Prim as Prim
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Nullable
import JavaScript.Object.Internal (Object(..))
import qualified JavaScript.Object as Object

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
defaultListenerOpts = ListenerOpts False False False

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
  hscb <- mkcallback \jsevent -> do
    when lo_prevent_default $ js_preventDefault jsevent
    when lo_stop_propagation $ js_stopPropagation jsevent
    f (DOMEvent jsevent)
  js_callMethod2 (coerce target) "addEventListener"
    (jsval (textToJSString name)) (jsval hscb)
  return do
    js_callMethod2 (coerce target) "removeEventListener"
      (jsval (textToJSString name)) (jsval hscb)
    releaseCallback hscb
  where
    mkcallback = if lo_sync_callback
      then syncCallback1 ThrowWouldBlock
      else asyncCallback1

-- | Collection of deltaX, deltaY and deltaZ properties from WheelEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
data MouseDelta = MouseDelta
  { md_delta_x :: Int
  , md_delta_y :: Int
  , md_delta_z :: Int
  } deriving stock (Eq, Show, Generic)

mMouseDelta :: MonadIO m => JSVal -> MaybeT m MouseDelta
mMouseDelta mouseEvent = do
  md_delta_x <- mGet "deltaX" mouseEvent
  md_delta_y <- mGet "deltaY" mouseEvent
  md_delta_z <- mGet "deltaZ" mouseEvent
  return MouseDelta {..}

-- | Collection of @X@ and @Y@ coordinates, intended to extract
data Vector2 a = Vector2
  { vector_x :: a
  , vector_y :: a
  } deriving stock (Eq, Show, Ord, Functor, Generic)

-- | Read clientX and clientY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
mClientXY :: MonadIO m => JSVal -> MaybeT m (Vector2 Int)
mClientXY mouseEvent = do
  vector_x <- mGet "clientX" mouseEvent
  vector_y <- mGet "clientY" mouseEvent
  return Vector2 {..}

-- | Read offsetX and offsetY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
mOffsetXY :: MonadIO m => JSVal -> MaybeT m (Vector2 Int)
mOffsetXY mouseEvent = do
  vector_x <- mGet "offsetX" mouseEvent
  vector_y <- mGet "offsetY" mouseEvent
  return Vector2 {..}

-- | Read pageX and pageY properties from MouseEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
mPageXY :: MonadIO m => JSVal -> MaybeT m (Vector2 Int)
mPageXY mouseEvent = do
  vector_x <- mGet "pageX" mouseEvent
  vector_y <- mGet "pageY" mouseEvent
  return Vector2 {..}

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
mKeyModifiers :: MonadIO m => JSVal -> MaybeT m KeyModifiers
mKeyModifiers keyEvent = do
  kmod_alt_key <- mGet "altKey" keyEvent
  kmod_ctrl_key <- mGet "ctrlKey" keyEvent
  kmod_meta_key <- mGet "metaKey" keyEvent
  kmod_shift_key <- mGet "shiftKey" keyEvent
  return KeyModifiers {..}

-- | Read keyCode properties from KeyboardEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode
mKeyCode :: MonadIO m => JSVal -> MaybeT m Int
mKeyCode = mGet "keyCode"

-- | Collection of some useful information from KeyboardEvent
data KeyboardEvent = KeyboardEvent
  { ke_modifiers :: KeyModifiers
  , ke_key :: Maybe Text
  , ke_key_code :: Int
  , ke_repeat :: Bool
  } deriving stock (Eq, Show, Generic)

-- | Read information from KeyboardEvent
mKeyboardEvent :: MonadIO m => JSVal -> MaybeT m KeyboardEvent
mKeyboardEvent keyEvent = do
  ke_modifiers <- mKeyModifiers keyEvent
  ke_key <- mGet "key" keyEvent
  ke_key_code <- mGet "keyCode" keyEvent
  ke_repeat <- mGet "repeat" keyEvent
  return KeyboardEvent {..}

-- | Event.target.value
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#attr-value
mTargetValue :: MonadIO m => JSVal -> MaybeT m Text
mTargetValue =
  mGet "target" >=> mGet "checked"

-- | Event.target.checked
-- https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/checkbox#checked
mTargetChecked :: MonadIO m => JSVal -> MaybeT m Bool
mTargetChecked =
  mGet "target" >=> mGet "checked"

mGet :: (MonadIO m, FromJSVal v) => Text -> JSVal -> MaybeT m v
mGet k obj = do
  -- TODO: Make sure it is true that if this guard succeeds,
  -- Object.getProp will never throw an exception
  guard $ not (isUndefined obj) && not (isNull obj)
  MaybeT $ liftIO $ fromJSVal =<<
    Object.getProp (textToJSString k) (coerce obj)

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
js_preventDefault :: JSVal -> IO () = errorGhcjsOnly
js_stopPropagation :: JSVal -> IO () = errorGhcjsOnly
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
  "$1.preventDefault()"
  js_preventDefault :: JSVal -> IO ()
foreign import javascript unsafe
  "$1.stopPropagation()"
  js_stopPropagation :: JSVal -> IO ()
#endif

instance (a ~ (), MonadIO m) => IsString (HtmlT m a) where
  fromString s = do
    HtmlEnv{..} <- ask
    textNode <- liftIO $ createTextNode (T.pack s)
    case html_content_boundary of
      Just ContentBoundary{..} -> liftIO $
        js_insertBefore html_current_element textNode boundary_end
      Nothing -> liftIO $ appendChild html_current_element textNode
