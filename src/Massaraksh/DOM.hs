{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.DOM where

import Data.Text
import GHC.Generics
import Language.Javascript.JSaddle
import Massaraksh.Decode

newtype Node = Node {unNode :: JSVal}
  deriving (MakeArgs, MakeObject, ToJSVal)

newtype Element = Element {unElement :: JSVal}
  deriving (MakeArgs, MakeObject, ToJSVal)

dUnit :: Decoder ()
dUnit = Decoder \_ -> pure (pure ())

dId :: Decoder SomeJVal
dId = Decoder $ pure . pure

dTarget :: Decoder SomeJVal
dTarget = parseAt ["target"] dId

dValue :: Decoder Text
dValue = parseAt ["target", "value"] decoder

dCurrentTarget :: Decoder SomeJVal
dCurrentTarget = parseAt ["currentTarget"] dId

dChecked :: Decoder Bool
dChecked = parseAt ["target", "checked"] decoder

data DeltaMouse = DeltaMouse
  { deltaX :: Int
  , deltaY :: Int
  , deltaZ :: Int
  } deriving (Eq, Show, Generic)

dDelta :: Decoder DeltaMouse
dDelta = DeltaMouse
  <$> parseAt ["deltaX"] decoder
  <*> parseAt ["deltaY"] decoder
  <*> parseAt ["deltaZ"] decoder

data Position = Position
  { x :: Int
  , y :: Int
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

data Keys = Keys
  { altKey   :: Bool
  , ctrlKey  :: Bool
  , metaKey  :: Bool
  , shiftKey :: Bool
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
  , key      :: !(Maybe Text)
  , keyCode  :: !Int
  , repeat   :: !Bool
  } deriving (Show, Eq)

dKeyboard :: Decoder KeyboardEvent
dKeyboard = KeyboardEvent
  <$> dKeys
  <*> parseAt ["key"] decoder
  <*> parseAt ["keyCode"] decoder
  <*> parseAt ["repeat"] decoder
