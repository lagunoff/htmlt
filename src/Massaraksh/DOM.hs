{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
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

#ifndef ghcjs_HOST_OS
appendChild :: Node -> Node -> JSM ()
appendChild root child = do
  void (root # ("appendChild" :: JSString) $ child)
#else
foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild :: Node -> Node -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
replaceChild :: Node -> Node -> Node -> JSM ()
replaceChild root new old = do
  void (root # ("replaceChild" :: JSString) $ (new, old))
#else
foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  replaceChild :: Node -> Node -> Node -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
createElement :: JSString -> JSM Node
createElement tag = do
  fmap coerce $ jsg ("document" :: JSString) # ("createElement" :: JSString) $ [tag]
#else
foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: JSString -> JSM Node
#endif

#ifndef ghcjs_HOST_OS
createElementNS :: JSString -> JSString -> JSM Node
createElementNS ns tag = do
  fmap coerce $ jsg ("document" :: JSString) # ("createElementNS" :: JSString) $ [ns, tag]
#else
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  createElementNS :: JSString -> JSString -> JSM Node
#endif

#ifndef ghcjs_HOST_OS
createTextNode :: JSString -> JSM Node
createTextNode tag = do
  fmap coerce $ jsg ("document" :: JSString) # ("createTextNode" :: JSString) $ [tag]
#else
foreign import javascript unsafe
  "document.createTextNode($1)"
  createTextNode :: JSString -> JSM Node
#endif

addEventListener :: JSVal -> JSString -> (JSVal -> JSM ()) -> JSM (JSM ())
addEventListener target name f = do
  cb <- function \_ _ [event] -> f event
  target # ("addEventListener" :: JSString) $ (name, cb)
  pure do
    target # ("removeEventListener" :: JSString) $ (name, cb)
    freeFunction cb

target :: Decoder JSVal
target = decodeAt ["target"] decodeJSVal

value :: Decoder JSString
value = decodeAt ["target", "value"] decoder

currentTarget :: Decoder JSVal
currentTarget = decodeAt ["currentTarget"] decodeJSVal

checked :: Decoder Bool
checked = decodeAt ["target", "checked"] decoder

data DeltaMouse = DeltaMouse
  { deltaX :: Int
  , deltaY :: Int
  , deltaZ :: Int }
  deriving stock (Eq, Show, Generic)

deltaMouse :: Decoder DeltaMouse
deltaMouse = DeltaMouse
  <$> decodeAt ["deltaX"] decoder
  <*> decodeAt ["deltaY"] decoder
  <*> decodeAt ["deltaZ"] decoder

data Position = Position
  { x :: Int
  , y :: Int }
  deriving stock (Eq, Show, Ord, Generic)

clientXY :: Decoder Position
clientXY = Position
  <$> decodeAt ["clientX"] decoder
  <*> decodeAt ["clientY"] decoder

offsetXY :: Decoder Position
offsetXY = Position
  <$> decodeAt ["offsetX"] decoder
  <*> decodeAt ["offsetY"] decoder

pageXY :: Decoder Position
pageXY = Position
  <$> decodeAt ["pageX"] decoder
  <*> decodeAt ["pageX"] decoder

data Keys = Keys
  { altKey   :: Bool
  , ctrlKey  :: Bool
  , metaKey  :: Bool
  , shiftKey :: Bool }
  deriving stock (Eq, Show, Generic)

keys :: Decoder Keys
keys = Keys
  <$> decodeAt ["altKey"] decoder
  <*> decodeAt ["ctrlKey"] decoder
  <*> decodeAt ["metaKey"] decoder
  <*> decodeAt ["shiftKey"] decoder

keyCode :: Decoder Int
keyCode = decodeAt ["keyCode"] decoder

data KeyboardEvent = KeyboardEvent
  { keys_    :: Keys
  , key      :: Maybe JSString
  , keyCode_ :: Int
  , repeat   :: Bool }
  deriving stock (Eq, Show, Generic)

keyboardEvent :: Decoder KeyboardEvent
keyboardEvent = KeyboardEvent
  <$> keys
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder
