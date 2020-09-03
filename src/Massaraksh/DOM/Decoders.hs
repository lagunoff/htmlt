{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.DOM.Decoders where

import Control.Applicative
import Data.Text as T
import GHC.Generics
import Language.Javascript.JSaddle as JS
import Massaraksh.Decode

target :: Decoder JSVal
target = decodeAt ["target"] decodeJSVal

value :: Decoder Text
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

clientXY :: Decoder (Int, Int)
clientXY = liftA2 (,)
  (decodeAt ["clientX"] decoder)
  (decodeAt ["clientY"] decoder)

offsetXY :: Decoder (Int, Int)
offsetXY = liftA2 (,)
  (decodeAt ["offsetX"] decoder)
  (decodeAt ["offsetY"] decoder)

pageXY :: Decoder (Int, Int)
pageXY = liftA2 (,)
  (decodeAt ["pageX"] decoder)
  (decodeAt ["pageY"] decoder)

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
  , key      :: Maybe Text
  , keyCode_ :: Int
  , repeat   :: Bool }
  deriving stock (Eq, Show, Generic)

keyboardEvent :: Decoder KeyboardEvent
keyboardEvent = KeyboardEvent
  <$> keys
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder
