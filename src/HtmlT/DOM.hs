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
import Data.Coerce
import Data.String
import Data.Text as T
import Data.JSString.Text
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Prim as Prim
import GHCJS.Marshal
import GHCJS.Types

import HtmlT.Decode
import HtmlT.Types

data ListenerOpts = ListenerOpts
  { lo_stop_propagation :: Bool
  , lo_prevent_default :: Bool
  , lo_sync_callback :: Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSVal)

type Decoding a = (a -> Html ()) -> DOMEvent -> Html ()

defaultListenerOpts :: ListenerOpts
defaultListenerOpts = ListenerOpts True False False

addEventListener
  :: ListenerOpts
  -> Node
  -> Text
  -> (JSVal -> IO ())
  -> IO (IO ())
addEventListener ListenerOpts{..} target name f = do
  cb <- mkcallback \event -> do
    when lo_stop_propagation do
      void $ jsCallMethod0 event "stopPropagation"
    when lo_prevent_default do
      void $ jsCallMethod0 event "preventDefault"
    f event
  jsCallMethod2 (coerce target) "addEventListener"
    (jsval (textToJSString name)) (jsval cb)
  return do
    jsCallMethod2 (coerce target) "removeEventListener"
      (jsval (textToJSString name)) (jsval cb)
    releaseCallback cb
  where
    mkcallback = if lo_sync_callback
      then syncCallback1 ThrowWouldBlock
      else asyncCallback1

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

data Position = Position
  { pos_x :: Int
  , pos_y :: Int
  } deriving stock (Eq, Show, Ord, Generic)

clientXYDecoder :: Decoder Position
clientXYDecoder = Position
  <$> decodeAt ["clientX"] decoder
  <*> decodeAt ["clientY"] decoder

offsetXYDecoder :: Decoder Position
offsetXYDecoder = Position
  <$> decodeAt ["offsetX"] decoder
  <*> decodeAt ["offsetY"] decoder

pageXYDecoder :: Decoder Position
pageXYDecoder = Position
  <$> decodeAt ["pageX"] decoder
  <*> decodeAt ["pageY"] decoder

data KeyModifiers = KeyModifiers
  { kmod_alt_key :: Bool
  , kmod_ctrl_key :: Bool
  , kmod_meta_key :: Bool
  , kmod_shift_key :: Bool
  } deriving stock (Eq, Show, Generic)

keyModifiersDecoder :: Decoder KeyModifiers
keyModifiersDecoder = KeyModifiers
  <$> decodeAt ["altKey"] decoder
  <*> decodeAt ["ctrlKey"] decoder
  <*> decodeAt ["metaKey"] decoder
  <*> decodeAt ["shiftKey"] decoder

keyCodeDecoder :: Decoder Int
keyCodeDecoder = decodeAt ["keyCode"] decoder

data KeyboardEvent = KeyboardEvent
  { ke_modifiers :: KeyModifiers
  , ke_key :: Maybe Text
  , ke_key_code :: Int
  , ke_repeat :: Bool
  } deriving stock (Eq, Show, Generic)

keyboardEventDecoder :: Decoder KeyboardEvent
keyboardEventDecoder = KeyboardEvent
  <$> keyModifiersDecoder
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder

decodeTarget :: Decoding JSVal
decodeTarget = withDecoder $
  decodeAt ["target"] decodeJSVal

decodeValue :: Decoding Text
decodeValue = withDecoder $
  decodeAt ["target", "value"] decoder

decodeCurrentTarget :: Decoding JSVal
decodeCurrentTarget = withDecoder $
  decodeAt ["currentTarget"] decodeJSVal

decodeChecked :: Decoding Bool
decodeChecked = withDecoder $
  decodeAt ["target", "checked"] decoder

decodeMouseDelta :: Decoding MouseDelta
decodeMouseDelta = withDecoder mouseDeltaDecoder

decodeKeyModifiers :: Decoding KeyModifiers
decodeKeyModifiers = withDecoder keyModifiersDecoder

decodeKeyCode :: Decoding Int
decodeKeyCode = withDecoder keyCodeDecoder

decodeOffsetXY :: Decoding Position
decodeOffsetXY = withDecoder offsetXYDecoder

decodeClientXY :: Decoding Position
decodeClientXY = withDecoder clientXYDecoder

decodePageXY :: Decoding Position
decodePageXY = withDecoder pageXYDecoder

decodeKeyboardEvent :: Decoding KeyboardEvent
decodeKeyboardEvent = withDecoder keyboardEventDecoder

getCurrentWindow :: MonadIO m => m JSVal
getCurrentDocument :: MonadIO m => m JSVal
getCurrentBody :: MonadIO m => m Node
appendChild :: Node -> Node -> IO ()
setAttribute :: Node -> Text -> Text -> IO ()
removeAttribute :: Node -> Text -> IO ()
removeChild :: Node -> Node -> IO ()
removeAllChilds :: Node -> IO ()
replaceChild :: Node -> Node -> Node -> IO ()
getChildNode :: Node -> Int -> IO Node
createElement :: Text -> IO Node
createElementNS :: Text -> Text -> IO Node
createTextNode :: Text -> IO Node
classListAdd :: Node -> Text -> IO ()
classListRemove :: Node -> Text -> IO ()
setTextValue :: Node -> Text -> IO ()
onBeforeUnload :: IO () -> IO ()
appendUnsafeHtml :: Node -> Text -> IO ()
jsCall0 :: JSVal -> IO JSVal
jsCall1 :: JSVal -> JSVal -> IO JSVal
jsCall2 :: JSVal -> JSVal -> JSVal -> IO JSVal
jsCallMethod0 :: JSVal -> JSString -> IO JSVal
jsCallMethod1 :: JSVal -> JSString -> JSVal -> IO JSVal
jsCallMethod2 :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal

errorGhcjsOnly :: a
errorGhcjsOnly = error "Only GHCJS is supported"

#ifndef ghcjs_HOST_OS
getCurrentWindow = errorGhcjsOnly
getCurrentDocument = errorGhcjsOnly
getCurrentBody = errorGhcjsOnly
appendChild _ _ = errorGhcjsOnly
setAttribute _ _ _ = errorGhcjsOnly
removeAttribute _ _ = errorGhcjsOnly
removeChild _ _ = errorGhcjsOnly
removeAllChilds _ = errorGhcjsOnly
replaceChild _ _ _ = errorGhcjsOnly
getChildNode _ _ = errorGhcjsOnly
createElement _ = errorGhcjsOnly
createElementNS _ _ = errorGhcjsOnly
createTextNode _ = errorGhcjsOnly
classListAdd _ _ = errorGhcjsOnly
classListRemove _ _ = errorGhcjsOnly
setTextValue _ _ = errorGhcjsOnly
onBeforeUnload _ = errorGhcjsOnly
appendUnsafeHtml = errorGhcjsOnly
jsCall0 _ = errorGhcjsOnly
jsCall1 _ _ = errorGhcjsOnly
jsCall2 _ _ _ = errorGhcjsOnly
jsCallMethod0 _ _ = errorGhcjsOnly
jsCallMethod1 _ _ _ = errorGhcjsOnly
jsCallMethod2 _ _ _ _ = errorGhcjsOnly
#else
getCurrentWindow = liftIO js_getCurrentWindow
getCurrentDocument = liftIO js_getCurrentDocument
getCurrentBody = liftIO $ fmap Node js_getCurrentBody
onBeforeUnload cb = do
  syncCb <- syncCallback ThrowWouldBlock cb
  js_onBeforeUnload syncCb
appendChild = js_appendChild
setAttribute e k v = js_setAttribute e (textToJSString k) (textToJSString v)
removeAttribute e k = js_removeAttribute e (textToJSString k)
removeChild = js_removeChild
removeAllChilds = js_removeAllChilds
replaceChild = js_replaceChild
getChildNode = js_getChildNode
createElement = js_createElement . textToJSString
createElementNS n t = js_createElementNS (textToJSString n) (textToJSString t)
createTextNode = js_createTextNode . textToJSString
classListAdd e c = js_classListAdd e (textToJSString c)
classListRemove e c = js_classListRemove e (textToJSString c)
setTextValue v = js_setTextValue v . textToJSString
appendUnsafeHtml n t = js_appendUnsafeHtml n (textToJSString t)
jsCall0 = js_jsCall0
jsCall1 = js_jsCall1
jsCall2 = js_jsCall2
jsCallMethod0 = js_jsCallMethod0
jsCallMethod1 = js_jsCallMethod1
jsCallMethod2 = js_jsCallMethod2

foreign import javascript unsafe
  "$1.appendChild($2)"
  js_appendChild :: Node -> Node -> IO ()
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  js_setAttribute :: Node -> JSString -> JSString -> IO ()
foreign import javascript unsafe
  "$1.removeAttribute($2)"
  js_removeAttribute :: Node -> JSString -> IO ()
foreign import javascript unsafe
  "$1.removeChild($2)"
  js_removeChild :: Node -> Node -> IO ()
foreign import javascript unsafe
  "$1.innerHTML = ''"
  js_removeAllChilds :: Node -> IO ()
foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  js_replaceChild :: Node -> Node -> Node -> IO ()
foreign import javascript unsafe
  "$1.childNodes[$2]"
  js_getChildNode :: Node -> Int -> IO Node
foreign import javascript unsafe
  "document.createElement($1)"
  js_createElement :: JSString -> IO Node
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  js_createElementNS :: JSString -> JSString -> IO Node
foreign import javascript unsafe
  "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO Node
foreign import javascript unsafe
  "$1.classList.add($2)"
  js_classListAdd :: Node -> JSString -> IO ()
foreign import javascript unsafe
  "$1.classList.remove($2)"
  js_classListRemove :: Node -> JSString -> IO ()
foreign import javascript unsafe
  "$1.nodeValue = $2;"
  js_setTextValue :: Node -> JSString -> IO ()
foreign import javascript unsafe
  "(function(cb){\
    window.addEventListener('beforeunload', function(e) {\
      delete e['returnEvent'];\
      cb();\
    })\
   })($1)"
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
foreign import javascript unsafe "$1()" js_jsCall0 :: JSVal -> IO JSVal
foreign import javascript unsafe "$1($2)" js_jsCall1 :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1($2, $3)" js_jsCall2 :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1[$2]()" js_jsCallMethod0 :: JSVal -> JSString -> IO JSVal
foreign import javascript unsafe "$1[$2]($3)" js_jsCallMethod1 :: JSVal -> JSString -> JSVal -> IO JSVal
foreign import javascript unsafe "$1[$2]($3, $4)" js_jsCallMethod2 :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe
  "(function(el, htmlString){\
    var div = document.createElement('div');\
    div.innerHTML = htmlString;\
    var tempChilds = [];\
    for (var i = 0; i < div.childNodes.length; i++) {\
      tempChilds.push(div.childNodes[i]);\
    }\
    for (var j = 0; j < tempChilds.length; j++) {\
      div.removeChild(tempChilds[j]);\
      el.appendChild(tempChilds[j]);\
    }\
  })($1, $2)"
  js_appendUnsafeHtml :: Node -> JSString -> IO ()
#endif

instance (x ~ (), MonadIO m) => IsString (HtmlT m x) where
  fromString = f . T.pack where
    f t = do
      rootEl <- asks html_current_root
      textNode <- liftIO (createTextNode t)
      liftIO (appendChild rootEl textNode)
