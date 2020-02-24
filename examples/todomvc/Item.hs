{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
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
--  , _propModel  :: Model
  } deriving (Show, Eq, Generic)
_propModel = id
data Model = Model
  { _moTitle     :: Text
  , _moCompleted :: Bool
  , _moEditing   :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSVal, FromJSVal)

makeLenses ''Model
makeLenses ''Props

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

itemWidget :: HtmlBase m => HtmlRecT Msg Model Model m
itemWidget yield = \case
  Init title ->
    pure (Model title False Nothing)
  Render -> do
    li_ do
      dynClassList
        [ ("completed", _moCompleted . _propModel)
        , ("editing", isJust . _moEditing . _propModel) ]
  --          , ("hidden", hidden) ]
      div_ do
        "className" =: "view"
        on "dblclick" $ targetDecoder <&> yield . EditingOn
      input_ do
        "className" =: "toggle"
        "type"      =: "checkbox"
        "checked"   ~: _moCompleted . _propModel
        on "change" $ checkedDecoder <&> yield . Completed
      label_ $ dynText (_moTitle . _propModel)
      button_ do
        "className" =: "destroy"
        on' "click" do yield Destroy
      input_ do
        "className" =: "edit"
        "value"     ~: fromMaybe "" . _moEditing .  _propModel
        on "input" $ valueDecoder <&> yield . EditInput
        on' "blur" $ yield Blur
        on "keydown" $ keycodeDecoder <&> yield . KeyPress
  Completed x ->
    omodify ((moCompleted .~ x) . _propModel)
  Destroy ->
    pure ()
  Blur ->
    yield EditingCommit
  KeyPress code -> do
    if | code == 13 -> yield EditingCommit -- Enter
       | code == 27 -> yield EditingCancel -- Escape
       | otherwise  -> pure ()
  EditingOn _ -> do
    model <- oget
    omodify ((moEditing .~ Just (_moTitle $ _propModel model)) . _propModel)
    -- FIXME: Set focus to the editing input
  EditInput x ->
    omodify $ (moEditing %~ fmap (const x)) . _propModel
  EditingCancel -> do
    omodify $ (moEditing .~ Nothing) . _propModel
  EditingCommit -> oget <&> (_moEditing .  _propModel) >>= \case
    Just "" -> yield Destroy
    Just x  -> omodify $ (moEditing .~ Nothing) . (moTitle .~ x) . _propModel
    Nothing -> pure ()
