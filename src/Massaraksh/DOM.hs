{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.DOM where

import Data.JSString
import Data.Coerce
import Data.Functor
import GHC.Generics
import Language.Javascript.JSaddle
import Massaraksh.Decode

newtype Node = Node {unNode :: JSVal}
  deriving newtype (MakeArgs, MakeObject, ToJSVal)

newtype Element = Element {unElement :: JSVal}
  deriving newtype (MakeArgs, MakeObject, ToJSVal)

type Fragment = Element

toNode :: Element -> Node
toNode = coerce
{-# INLINE toNode #-}

#ifndef ghcjs_HOST_OS
appendChild :: Element -> Element -> JSM ()
appendChild root child = do
  void (root # ("appendChild" :: JSString) $ child)
#else
foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild :: Element -> Element -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
createDocumentFragment :: JSM Fragment
createDocumentFragment = do
  fmap coerce $ jsg ("document" :: JSString) # ("createDocumentFragment" :: JSString) $ ()
#else
foreign import javascript unsafe
  "document.createDocumentFragment()"
  createDocumentFragment :: JSM Fragment
#endif

#ifndef ghcjs_HOST_OS
replaceChild :: Element -> Element -> Element -> JSM ()
replaceChild root new old = do
  void (root # ("replaceChild" :: JSString) $ (new, old))
#else
foreign import javascript unsafe "$1.replaceChild($2, $3)" replaceChild :: Element -> Element -> Element -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
createElement :: JSString -> JSM Element
createElement tag = do
  fmap coerce $ jsg ("document" :: JSString) # ("createElement" :: JSString) $ [tag]
#else
foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: JSString -> JSM Element
#endif

#ifndef ghcjs_HOST_OS
createElementNS :: JSString -> JSString -> JSM Element
createElementNS ns tag = do
  fmap coerce $ jsg ("document" :: JSString) # ("createElementNS" :: JSString) $ [ns, tag]
#else
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  createElementNS :: JSString -> JSString -> JSM Element
#endif

#ifndef ghcjs_HOST_OS
createTextNode :: JSString -> JSM Element
createTextNode tag = do
  fmap coerce $ jsg ("document" :: JSString) # ("createTextNode" :: JSString) $ [tag]
#else
foreign import javascript unsafe
  "document.createTextNode($1)"
  createTextNode :: JSString -> JSM Element
#endif


dUnit :: Decoder ()
dUnit = Decoder \_ -> pure (pure ())

dId :: Decoder SomeJVal
dId = Decoder $ pure . pure

dTarget :: Decoder SomeJVal
dTarget = parseAt ["target"] dId

dValue :: Decoder JSString
dValue = parseAt ["target", "value"] decoder

dCurrentTarget :: Decoder SomeJVal
dCurrentTarget = parseAt ["currentTarget"] dId

dChecked :: Decoder Bool
dChecked = parseAt ["target", "checked"] decoder

data DeltaMouse = DeltaMouse {
  deltaX :: Int,
  deltaY :: Int,
  deltaZ :: Int
} deriving (Eq, Show, Generic)

dDelta :: Decoder DeltaMouse
dDelta = DeltaMouse
  <$> parseAt ["deltaX"] decoder
  <*> parseAt ["deltaY"] decoder
  <*> parseAt ["deltaZ"] decoder

data Position = Position {
  x :: Int,
  y :: Int
} deriving (Eq, Show, Ord, Generic)

dClientPos :: Decoder Position
dClientPos = Position
  <$> parseAt ["clientX"] decoder
  <*> parseAt ["clientY"] decoder

dOffsetPos :: Decoder Position
dOffsetPos = Position
  <$> parseAt ["offsetX"] decoder
  <*> parseAt ["offsetY"] decoder

dPagePos :: Decoder Position
dPagePos = Position
  <$> parseAt ["pageX"] decoder
  <*> parseAt ["pageX"] decoder

data Keys = Keys {
  altKey   :: Bool,
  ctrlKey  :: Bool,
  metaKey  :: Bool,
  shiftKey :: Bool
} deriving (Eq, Show, Generic)

dKeys :: Decoder Keys
dKeys = Keys
  <$> parseAt ["altKey"] decoder
  <*> parseAt ["ctrlKey"] decoder
  <*> parseAt ["metaKey"] decoder
  <*> parseAt ["shiftKey"] decoder

dKeyCode :: Decoder Int
dKeyCode = parseAt ["keyCode"] decoder

data KeyboardEvent = KeyboardEvent
  { keys     :: !Keys
  , key      :: !(Maybe JSString)
  , keyCode  :: !Int
  , repeat   :: !Bool
  } deriving (Show, Eq)

dKeyboard :: Decoder KeyboardEvent
dKeyboard = KeyboardEvent
  <$> dKeys
  <*> parseAt ["key"] decoder
  <*> parseAt ["keyCode"] decoder
  <*> parseAt ["repeat"] decoder
