module TodoItem where

import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import GHC.Int
import Clickable
import Clickable.Protocol (VarId)
import Clickable.Protocol.Value

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
  } deriving stock (Show, Eq)

data TodoItemAction a where
  CancelAction :: TodoItemAction ()
  CommitAction :: TodoItemAction ()
  InputAction :: Text -> TodoItemAction ()
  DoubleClickAction :: VarId -> TodoItemAction ()
  CheckedAction :: Bool -> TodoItemAction ()
  KeydownAction :: Int64 -> TodoItemAction ()

emptyState :: TodoItemState
emptyState = TodoItemState "" False Nothing

eval :: TodoItemConfig -> TodoItemAction a -> ClickM a
eval cfg = \case
  CancelAction ->
    modifyVar_ cfg.state_var \s -> s{editing=Nothing}
  CommitAction -> do
    state <- readVar cfg.state_var
    case state.editing of
      Just "" ->
        cfg.ask_delete_item
      Just t ->
        modifyVar_ cfg.state_var \s -> s {editing=Nothing, title = t}
      Nothing ->
        pure ()
  InputAction newVal ->
    modifyVar_ cfg.state_var \s -> s{editing = Just newVal}
  DoubleClickAction inpElm -> do
    trampoline do
      modifyVar_ cfg.state_var \s -> s {editing = Just s.title}
      syncPoint
    assignFocus inpElm
  CheckedAction isChecked -> do
    modifyVar_ cfg.state_var \s -> s{completed = isChecked}
  KeydownAction key -> case key of
    13 {- Enter -} -> eval cfg CommitAction
    27 {- Escape -} -> eval cfg CancelAction
    _ -> return ()

html :: TodoItemConfig -> HtmlM ()
html cfg = li_ mdo
  let
    completedDyn = (.completed) <$> fromVar cfg.state_var
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
    saveCurrentNode
  return ()

instance ToJSValue TodoItemState where
  toJSValue s = Object
    [ ("title", toJSValue s.title)
    , ("completed", toJSValue s.completed)
    ]

instance FromJSValue TodoItemState where
  fromJSValue (Object kv) = do
    title <- fromJSValue =<< List.lookup "title" kv
    completed <- fromJSValue =<< List.lookup "completed" kv
    return TodoItemState {editing=Nothing, ..}
  fromJSValue _ = Nothing
