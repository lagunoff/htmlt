module TodoItem where

import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import GHC.Int
import Clickable
import Clickable.Protocol.Value

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
  CancelAction :: TodoItemConfig -> TodoItemAction ()
  CommitAction :: TodoItemConfig -> TodoItemAction ()
  InputAction :: TodoItemConfig -> Text -> TodoItemAction ()
  DoubleClickAction :: TodoItemConfig -> TodoItemAction ()
  CheckedAction :: TodoItemConfig -> Bool -> TodoItemAction ()
  KeydownAction :: TodoItemConfig -> Int64 -> TodoItemAction ()

emptyState :: TodoItemState
emptyState = TodoItemState "" False Nothing

eval :: TodoItemAction a -> ClickM a
eval = \case
  CancelAction cfg ->
    modifyVar_ cfg.state_var \s -> s{editing=Nothing}
  CommitAction cfg -> do
    state <- readVar cfg.state_var
    case state.editing of
      Just "" ->
        cfg.ask_delete_item
      Just t ->
        modifyVar_ cfg.state_var \s -> s {editing=Nothing, title = t}
      Nothing ->
        pure ()
  InputAction cfg newVal ->
    modifyVar_ cfg.state_var \s -> s{editing = Just newVal}
  DoubleClickAction cfg -> do
    modifyVar_ cfg.state_var \s -> s {editing = Just s.title}
--    liftIO $ js_todoItemInputFocus targetEl
  CheckedAction cfg isChecked -> do
    modifyVar_ cfg.state_var \s -> s{completed = isChecked}
  KeydownAction cfg key -> case key of
    13 {- Enter -} -> eval (CommitAction cfg)
    27 {- Escape -} -> eval (CancelAction cfg)
    _ -> return ()

html :: TodoItemConfig -> HtmlM ()
html cfg = li_ do
  let
    completedDyn = (.completed) <$> fromVar cfg.state_var
    editingDyn = isJust . (.editing) <$> fromVar cfg.state_var
    valueDyn = fromMaybe "" . (.editing) <$> fromVar cfg.state_var
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden_dyn
  div_ [class_ "view"] do
    on @"dblclick" $ eval $ DoubleClickAction cfg
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.completed) <$> fromVar cfg.state_var
      on @"checkbox/change" $ eval . CheckedAction cfg
    label_ $ dynText $ (.title) <$> fromVar cfg.state_var
    button_ [class_ "destroy"] do
      on @"click" cfg.ask_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on @"input" $ eval . InputAction cfg
    on @"keydown" $ eval . KeydownAction cfg
    on @"blur" $ eval (CommitAction cfg)

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
