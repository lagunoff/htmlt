{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module TodoItem where

import Clickable
import Data.Maybe
import Data.Text (Text)
import GHC.Int
import GHC.Generics

import Utils

data TodoItemConfig = TodoItemConfig {
  self :: DynVar TodoItemState,
  is_hidden_dyn :: Dynamic Bool,
  ask_delete_item :: JSM ()
}

data TodoItemState = TodoItemState {
  title :: Text,
  completed :: Bool,
  editing :: Maybe Text
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

data TodoItemAction a where
  CancelAction :: TodoItemAction ()
  CommitAction :: TodoItemAction ()
  InputAction :: Text -> TodoItemAction ()
  DoubleClickAction :: RefId -> TodoItemAction ()
  CheckedAction :: Bool -> TodoItemAction ()
  KeydownAction :: Int32 -> TodoItemAction ()

emptyState :: TodoItemState
emptyState = TodoItemState "" False Nothing

eval :: TodoItemConfig -> TodoItemAction a -> JSM a
eval cfg CancelAction =
  modifyVar_ cfg.self \s -> s{editing=Nothing}
eval cfg CommitAction = do
  state <- readVar cfg.self
  case state.editing of
    Just "" ->
      cfg.ask_delete_item
    Just t ->
      modifyVar_ cfg.self \s -> s {editing=Nothing, title = t}
    Nothing ->
      pure ()
eval cfg (InputAction newVal) =
  modifyVar_ cfg.self \s -> s{editing = Just newVal}
eval cfg (DoubleClickAction inpElm) = do
  modifyVar_ cfg.self \s -> s {editing = Just s.title}
  assignFocus inpElm
eval cfg (CheckedAction isChecked) =
  modifyVar_ cfg.self \s -> s{completed = isChecked}
eval cfg (KeydownAction key) = case key of
  13 {- Enter -} -> eval cfg CommitAction
  27 {- Escape -} -> eval cfg CancelAction
  _ -> return ()

view :: TodoItemConfig -> HTML ()
view cfg = li_ mdo
  let completedDyn = (.completed) <$> fromVar cfg.self
      editingDyn = isJust . (.editing) <$> fromVar cfg.self
      valueDyn = fromMaybe "" . (.editing) <$> fromVar cfg.self
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden_dyn
  div_ [class_ "view"] do
    on @"dblclick" $ eval cfg $ DoubleClickAction inp
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.completed) <$> fromVar cfg.self
      on @"checkbox/change" $ eval cfg . CheckedAction
    label_ $ dynText $ (.title) <$> fromVar cfg.self
    button_ [class_ "destroy"] do
      on @"click" cfg.ask_delete_item
  inp <- input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on @"input" $ eval cfg . InputAction
    on @"keydown" $ eval cfg . KeydownAction
    on @"blur" $ eval cfg CommitAction
    saveStackHead
  return ()
