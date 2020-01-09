{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf, CPP, TemplateHaskell #-}
module Item where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import GHCJS.Types
import GHCJS.Marshal
import Massaraksh

data Props = Props
  { _propHidden :: Bool
  } deriving (Show, Eq, Generic)

makeLenses ''Props

data Model = Model
  { _moTitle     :: Text
  , _moCompleted :: Bool
  , _moEditing   :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSVal, FromJSVal)

makeLenses ''Model

data Msg a where
  Init :: Text -> Msg Model
  Render :: Msg ()
  Completed :: Bool -> Msg ()
  Destroy :: Msg ()
  Blur :: Msg ()
  EditingOn :: JSVal -> Msg ()
  EditInput :: Text -> Msg ()
  KeyPress :: Int -> Msg ()
  EditingCancel :: Msg ()
  EditingCommit :: Msg ()

component :: forall m. MonadHtmlBase m => Msg ~> ComponentT Msg Model Model m
component = \case
  Init title ->
    pure (Model title False Nothing)
  Render ->
    lift render
  Completed x ->
    modify (moCompleted .~ x)
  Destroy ->
    pure ()
  Blur ->
    yield1 EditingCommit
  KeyPress code -> do
    if | code == 13 -> yield1 EditingCommit -- Enter
       | code == 27 -> yield1 EditingCancel -- Escape
       | otherwise  -> pure ()
  EditingOn _ -> do
    model <- get
    modify (moEditing .~ Just (_moTitle model))
    -- FIXME: Set focus to the editing input
  EditInput x ->
    modify $ moEditing %~ fmap (const x)
  EditingCancel -> do
    modify $ moEditing .~ Nothing
  EditingCommit -> gets _moEditing >>= \case
    Just "" -> yield1 Destroy
    Just x  -> modify $ (moEditing .~ Nothing) . (moTitle .~ x)
    Nothing -> pure ()
  where
    render :: HtmlT Msg Model Model m ()
    render =
      li_ do
        dynClassList
          [ ("completed", _moCompleted)
          , ("editing", isJust . _moEditing) ]
--          , ("hidden", hidden) ]
        div_ do
          "className" =: "view"
          on "dblClick" $ targetDecoder <&> yield1 . EditingOn
        input_ do
          "className" =: "toggle"
          "type" =: "checkbox"
          "checked" ~: _moCompleted
          on "change" $ checkedDecoder <&> yield1 . Completed
        label_ $ dynText _moTitle
        button_ do
          "className" =: "destroy"
          on1 "click" $ yield1 Destroy
        input_ do
          "className" =: "edit"
          "value" ~: fromMaybe "" . _moEditing
          on "input" $ valueDecoder <&> yield1 . EditInput
          on1 "blur" $ yield1 Blur
          on "keydown" $ keycodeDecoder <&> yield1 . KeyPress
