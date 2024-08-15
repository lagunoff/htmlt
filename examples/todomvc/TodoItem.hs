module TodoItem where

import Clickable
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import GHC.Int
import GHC.Generics

import "this" Utils

data TodoItemConfig = TodoItemConfig
  { state_var :: DynVar TodoItemState
  , is_hidden_dyn :: DynVal Bool
  , ask_delete_item :: ClickM ()
  }

data TodoItemState = TodoItemState
  { title :: Text
  , completed :: Bool
  , editing :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromValue, ToValue)

data TodoItemAction a where
  CancelAction :: TodoItemAction ()
  CommitAction :: TodoItemAction ()
  InputAction :: Text -> TodoItemAction ()
  DoubleClickAction :: VarId -> TodoItemAction ()
  CheckedAction :: Bool -> TodoItemAction ()
  KeydownAction :: Int32 -> TodoItemAction ()

emptyState :: TodoItemState
emptyState = TodoItemState "" False Nothing

eval :: TodoItemConfig -> TodoItemAction a -> ClickM a
eval cfg CancelAction =
  modifyVar_ cfg.state_var \s -> s{editing=Nothing}
eval cfg CommitAction = do
  state <- readVar cfg.state_var
  case state.editing of
    Just "" ->
      cfg.ask_delete_item
    Just t ->
      modifyVar_ cfg.state_var \s -> s {editing=Nothing, title = t}
    Nothing ->
      pure ()
eval cfg (InputAction newVal) =
  modifyVar_ cfg.state_var \s -> s{editing = Just newVal}
eval cfg (DoubleClickAction inpElm) = do
  trampoline do
    modifyVar_ cfg.state_var \s -> s {editing = Just s.title}
    syncPoint
  assignFocus inpElm
eval cfg (CheckedAction isChecked) =
  modifyVar_ cfg.state_var \s -> s{completed = isChecked}
eval cfg (KeydownAction key) = case key of
  13 {- Enter -} -> eval cfg CommitAction
  27 {- Escape -} -> eval cfg CancelAction
  _ -> return ()

html :: TodoItemConfig -> HtmlM ()
html cfg = li_ mdo
  let completedDyn = (.completed) <$> fromVar cfg.state_var
      editingDyn = isJust . (.editing) <$> fromVar cfg.state_var
      valueDyn = fromMaybe "" . (.editing) <$> fromVar cfg.state_var
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden_dyn
  div_ [class_ "view"] do
    on @"dblclick" $ eval cfg $ DoubleClickAction inp
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.completed) <$> fromVar cfg.state_var
      on @"checkbox/change" $ eval cfg . CheckedAction
    label_ $ dynText $ (.title) <$> fromVar cfg.state_var
    button_ [class_ "destroy"] do
      on @"click" cfg.ask_delete_item
  inp <- input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on @"input" $ eval cfg . InputAction
    on @"keydown" $ eval cfg . KeydownAction
    on @"blur" $ eval cfg CommitAction
    saveDomBuilder
  return ()
