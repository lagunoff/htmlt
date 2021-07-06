-- | Functions and definitions to work with DOM. ghcjs-dom is too
-- heavy, takes about an hour to compile and increases size of the
-- generated JavaScript
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
  let
    mkcallback = if lo_sync_callback
      then syncCallback1 ThrowWouldBlock
      else asyncCallback1
  cb <- mkcallback \event -> do
    when lo_stop_propagation do
      void $ jscall0 event "stopPropagation"
    when lo_prevent_default do
      void $ jscall0 event "preventDefault"
    f event
  jscall2 (coerce target) "addEventListener" (jsval (textToJSString name)) (jsval cb)
  return do
    jscall2 (coerce target) "removeEventListener" (jsval (textToJSString name)) (jsval cb)
    releaseCallback cb

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
getCurrentWindow = liftIO js_getWindow

getCurrentDocument :: MonadIO m => m JSVal
getCurrentDocument = liftIO js_getDocument

getCurrentBody :: MonadIO m => m Node
getCurrentBody = liftIO $ coerce <$> js_getBody

instance (x ~ (), MonadIO m) => IsString (HtmlT m x) where
  fromString = f . T.pack where
    f t = do
      rootEl <- asks html_current_root
      textNode <- liftIO (createTextNode t)
      liftIO (appendChild rootEl textNode)

#ifndef ghcjs_HOST_OS
appendChild :: Node -> Node -> IO ()
appendChild root child = error "Only GHCJS is supported"
setAttribute :: Node -> Text -> Text -> IO ()
setAttribute e k v =  error "Only GHCJS is supported"
removeAttribute :: Node -> Text -> IO ()
removeAttribute e k =  error "Only GHCJS is supported"
removeChild :: Node -> Node -> IO ()
removeChild p ch = error "Only GHCJS is supported"
removeAllChilds :: Node -> IO ()
removeAllChilds e = error "Only GHCJS is supported"
replaceChild :: Node -> Node -> Node -> IO ()
replaceChild root new old = error "Only GHCJS is supported"
getChildNode :: Node -> Int -> IO Node
getChildNode e ix = error "Only GHCJS is supported"
createElement :: Text -> IO Node
createElement tag = error "Only GHCJS is supported"
createElementNS :: Text -> Text -> IO Node
createElementNS ns tag = error "Only GHCJS is supported"
createTextNode :: Text -> IO Node
createTextNode tag = error "Only GHCJS is supported"
classListAdd :: Node -> Text -> IO ()
classListAdd e c = error "Only GHCJS is supported"
classListRemove :: Node -> Text -> IO ()
classListRemove e c = error "Only GHCJS is supported"
setTextValue :: Node -> Text -> IO ()
setTextValue e c = error "Only GHCJS is supported"
onBeforeUnload :: IO () -> IO ()
onBeforeUnload _ = error "Only GHCJS is supported"
js0 :: JSVal -> IO JSVal
js0 _ = error "Only GHCJS is supported"
js1 :: JSVal -> JSVal -> IO JSVal
js1 _ _ = error "Only GHCJS is supported"
js2 :: JSVal -> JSVal -> JSVal -> IO JSVal
js2 _ _ _ = error "Only GHCJS is supported"
jscall0 :: JSVal -> JSString -> IO JSVal
jscall0 _ _ = error "Only GHCJS is supported"
jscall1 :: JSVal -> JSString -> JSVal -> IO JSVal
jscall1 _ _ _ = error "Only GHCJS is supported"
jscall2 :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal
jscall2 _ _ _ _ = error "Only GHCJS is supported"
js_getWindow :: IO JSVal
js_getWindow = error "Only GHCJS is supported"
js_getDocument :: IO JSVal
js_getDocument = error "Only GHCJS is supported"
js_getBody :: IO Node
js_getBody = error "Only GHCJS is supported"
#else
foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild :: Node -> Node -> IO ()
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  setAttribute :: Node -> Text -> Text -> IO ()
foreign import javascript unsafe
  "$1.removeAttribute($2)"
  removeAttribute :: Node -> Text -> IO ()
foreign import javascript unsafe
  "$1.removeChild($2)"
  removeChild :: Node -> Node -> IO ()
foreign import javascript unsafe
  "$1.innerHTML = ''"
  removeAllChilds :: Node -> IO ()
foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  replaceChild :: Node -> Node -> Node -> IO ()
foreign import javascript unsafe
  "$1.childNodes[$2]"
  getChildNode :: Node -> Int -> IO Node
foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: Text -> IO Node
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  createElementNS :: Text -> Text -> IO Node
foreign import javascript unsafe
  "document.createTextNode($1)"
  createTextNode :: Text -> IO Node
foreign import javascript unsafe
  "$1.classList.add($2)"
  classListAdd :: Node -> Text -> IO ()
foreign import javascript unsafe
  "$1.classList.remove($2)"
  classListRemove :: Node -> Text -> IO ()
foreign import javascript unsafe
  "$1.nodeValue = $2;"
  setTextValue :: Node -> Text -> IO ()
foreign import javascript unsafe
  "(function(cb){\
    window.addEventListener('beforeunload', function(e) {\
      delete e['returnEvent'];\
      cb();\
    })\
   })($1)"
  js_onBeforeUnload :: Callback a -> IO ()
onBeforeUnload :: IO () -> IO ()
onBeforeUnload cb = do
  syncCb <- syncCallback ThrowWouldBlock cb
  js_onBeforeUnload syncCb
foreign import javascript unsafe "$1()" js0 :: JSVal -> IO ()
foreign import javascript unsafe "$1($2)" js1 :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1($2, $3)" js2 :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "(function(){ return window; })()" js_getWindow :: IO JSVal
foreign import javascript unsafe "(function(){ return window.document; })()" js_getDocument :: IO JSVal
foreign import javascript unsafe "(function(){ return window.document.body; })()" js_getBody :: IO JSVal
foreign import javascript unsafe "$1[$2]()" jscall0 :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "$1[$2]($3)" jscall1 :: JSVal -> JSString -> JSVal -> IO ()
foreign import javascript unsafe "$1[$2]($3, $4)" jscall2 :: JSVal -> JSString -> JSVal -> JSVal -> IO ()
#endif
