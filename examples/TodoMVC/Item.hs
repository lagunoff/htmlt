{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf, CPP #-}
module TodoMVC.Item where

import GHC.Generics
import GHCJS.Types (JSVal)
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Aeson
import Control.Lens
import Massaraksh.Html.Exists
import Massaraksh.Html.Element
import Massaraksh.Html.Attrs
import qualified Massaraksh.Html.Attrs.Dynamic as Dyn
import qualified GHCJS.DOM.GlobalEventHandlers as E
import Polysemy
import Polysemy.State
import Massaraksh.Component
import Data.Generics.Product (field)

data Props = Props
  { hidden :: Bool
  , model  :: Model
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Model = Model
  { title     :: Text
  , completed :: Bool
  , editing   :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Msg a where
  Completed :: Bool -> Msg ()
  Destroy :: Msg ()
  Blur :: Msg ()
  EditingOn :: JSVal -> Msg ()
  EditInput :: Text -> Msg ()
  KeyPress :: Int -> Msg ()
  EditingCancel :: Msg ()
  EditingCommit :: Msg ()
  
init :: Text -> Model
init title =
  Model title False Nothing
  
eval :: Members '[State Model, Emit Msg, Embed IO] r => Msg a -> Sem r a
eval = \case
  Completed x ->
    modify @Model $ field @"completed" .~ x
  Destroy ->
    pure ()
  Blur -> emit EditingCommit
  KeyPress code -> do
    if | code == 13 -> emit EditingCommit -- Enter
       | code == 27 -> emit EditingCancel -- Escape
       | otherwise -> pure ()
  EditingOn _ -> do
    model <- get
    modify @Model $ field @"editing" .~ Just (title model)
  EditInput x ->
    modify @Model $ field @"editing" %~ fmap (const x)
  EditingCancel -> do
    modify @Model $ field @"editing" .~ Nothing
  EditingCommit -> do
    model <- get
    case editing model of
      Just ""  -> emit Destroy
      Just str -> modify @Model $ (field @"editing" .~ Nothing) . (field @"title" .~ str)
      Nothing  -> pure ()

view :: Html1 Msg Props Model
view =
  li_
  [ Dyn.classList_
    [ ("completed", completed . model)
    , ("editing", isJust . editing . model)
    , ("hidden", hidden)
    ]
  ]
  [ div_
    [ class_ "view"
    , on1_ E.dblClick (EditingOn undefined)
    ]
    [ input_
      [ class_ "toggle"
      , type_ "checkbox"
      , onWithOptions1_ E.change checkedDecoder Completed
      , Dyn.checked_ (completed . model)
      ]
    , label_ [] [ textDyn (title . model) ]
    , button_ [ class_ "destroy", on1_ E.click Destroy ] []
    ]
  , input_
    [ class_ "edit"
    , Dyn.value_ $ maybe "" id . editing . model
    , onInput_ (Exists . EditInput)
    , on1_ E.blur Blur
    , onWithOptions1_ E.keyDown keycodeDecoder KeyPress
    ]
  ]
