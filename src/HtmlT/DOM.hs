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
import GHC.Generics
import Language.Javascript.JSaddle as JS
import Debug.Trace

import HtmlT.Decode
import HtmlT.Types

data ListenerOpts = ListenerOpts
  { lo_stop_propagation :: Bool
  , lo_prevent_default :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal)

type Decoding a = (a -> HtmlT ()) -> DOMEvent -> HtmlT ()

defaultListenerOpts :: ListenerOpts
defaultListenerOpts = ListenerOpts True False

#ifndef ghcjs_HOST_OS
appendChild :: Node -> Node -> JSM ()
appendChild root child = do
  void (root # ("appendChild"::Text) $ child)
setAttribute :: Node -> Text -> Text -> JSM ()
setAttribute e k v = do
  void $ e # ("setAttribute"::Text) $ (k, v)
removeAttribute :: Node -> Text -> JSM ()
removeAttribute e k = do
  void $ e # ("removeAttribute"::Text) $ [k]
removeChild :: Node -> Node -> JSM ()
removeChild p ch = do
  void $ p # ("removeChild"::Text) $ [ch]
removeAllChilds :: Node -> JSM ()
removeAllChilds e = do
  void $ e <# ("innerHTML"::Text) $ (""::Text)
replaceChild :: Node -> Node -> Node -> JSM ()
replaceChild root new old = do
  void (root # ("replaceChild"::Text) $ (new, old))
getChildNode :: Node -> Int -> JSM Node
getChildNode e ix =
  fmap coerce (e ! ("childNodes"::Text) JS.!! ix)
createElement :: Text -> JSM Node
createElement tag = do
  fmap coerce $ jsg ("document"::Text) # ("createElement"::Text) $ [tag]
createElementNS :: Text -> Text -> JSM Node
createElementNS ns tag = do
  fmap coerce $ jsg ("document"::Text) # ("createElementNS"::Text) $ [ns, tag]
createTextNode :: Text -> JSM Node
createTextNode tag = do
  fmap coerce $ jsg ("document"::Text) # ("createTextNode"::Text) $ [tag]
classListAdd :: Node -> Text -> JSM ()
classListAdd e c = do
  void $ e ! ("classList"::Text) # ("add"::Text) $ [c]
classListRemove :: Node -> Text -> JSM ()
classListRemove e c = do
  void $ e ! ("classList"::Text) # ("remove"::Text) $ [c]
setTextValue :: Node -> Text -> JSM ()
setTextValue e c = do
  void $ e <# ("nodeValue"::Text) $ c
#else
foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild :: Node -> Node -> JSM ()
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  setAttribute :: Node -> Text -> Text -> JSM ()
foreign import javascript unsafe
  "$1.removeAttribute($2)"
  removeAttribute :: Node -> Text -> JSM ()
foreign import javascript unsafe
  "$1.removeChild($2)"
  removeChild :: Node -> Node -> JSM ()
foreign import javascript unsafe
  "$1.innerHTML = ''"
  removeAllChilds :: Node -> JSM ()
foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  replaceChild :: Node -> Node -> Node -> JSM ()
foreign import javascript unsafe
  "$1.childNodes[$2]"
  getChildNode :: Node -> Int -> JSM Node
foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: Text -> JSM Node
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  createElementNS :: Text -> Text -> JSM Node
foreign import javascript unsafe
  "document.createTextNode($1)"
  createTextNode :: Text -> JSM Node
foreign import javascript unsafe
  "$1.classList.add($2)"
  classListAdd :: Node -> Text -> JSM ()
foreign import javascript unsafe
  "$1.classList.remove($2)"
  classListRemove :: Node -> Text -> JSM ()
foreign import javascript unsafe
  "$1.nodeValue = $2;"
  setTextValue :: Node -> Text -> JSM ()
#endif

addEventListener
  :: ListenerOpts
  -> Node
  -> Text
  -> (JSVal -> JSM ())
  -> JSM (JSM ())
addEventListener ListenerOpts{..} target name f = do
  cb <- function \_ _ [event] -> do
    traceShowM $ "addEventListener" <> name
    when lo_stop_propagation do
      void $ event # ("stopPropagation"::Text) $ ()
    when lo_prevent_default do
      void $ event # ("preventDefault"::Text) $ ()
    f event
  target # ("addEventListener"::Text) $ (name, cb)
  pure do
    target # ("removeEventListener"::Text) $ (name, cb)
    freeFunction cb

data MouseDelta = MouseDelta
  { md_delta_x :: Int
  , md_delta_y :: Int
  , md_delta_z :: Int
  }
  deriving stock (Eq, Show, Generic)

mouseDeltaDecoder :: Decoder MouseDelta
mouseDeltaDecoder = MouseDelta
  <$> decodeAt ["deltaX"] decoder
  <*> decodeAt ["deltaY"] decoder
  <*> decodeAt ["deltaZ"] decoder

data Position = Position
  { pos_x :: Int
  , pos_y :: Int
  }
  deriving stock (Eq, Show, Ord, Generic)

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
  }
  deriving stock (Eq, Show, Generic)

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
  }
  deriving stock (Eq, Show, Generic)

keyboardEventDecoder :: Decoder KeyboardEvent
keyboardEventDecoder = KeyboardEvent
  <$> keyModifiersDecoder
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder

decodeTarget :: Decoding JSVal
decodeTarget = withDecoder d where
  d = decodeAt ["target"] decodeJSVal

decodeValue :: Decoding Text
decodeValue = withDecoder d where
  d = decodeAt ["target", "value"] decoder

decodeCurrentTarget :: Decoding JSVal
decodeCurrentTarget = withDecoder d where
  d = decodeAt ["currentTarget"] decodeJSVal

decodeChecked :: Decoding Bool
decodeChecked = withDecoder d where
  d = decodeAt ["target", "checked"] decoder

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

getCurrentWindow :: MonadJSM m => m JSVal
getCurrentWindow = liftJSM (jsg ("window"::Text))

getDocument :: MonadJSM m => JSVal -> m JSVal
getDocument self = liftJSM (self ! ("document"::Text))

getCurrentBody :: MonadJSM m => m Node
getCurrentBody = liftJSM (Node <$> jsg ("document"::Text) ! ("body"::Text))

instance (x ~ ()) => IsString (HtmlT x) where
  fromString = text . T.pack where
    text t = do
      elm <- liftIO =<< asks (nr_read . he_current_root)
      textNode <- liftJSM (createTextNode t)
      liftJSM (appendChild elm textNode)
