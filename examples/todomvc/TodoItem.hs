module TodoItem where

import Control.Monad.State
import Data.Maybe
import GHC.Generics (Generic)
import HtmlT
import JavaScript.Compat.Marshal
import JavaScript.Compat.Prim
import JavaScript.Compat.String (JSString(..))

import "this" Utils

data TodoItemConfig = TodoItemConfig
  { state_ref :: DynRef TodoItemState
  , is_hidden_dyn :: Dynamic Bool
  , ask_delete_item :: Step ()
  }

data TodoItemState = TodoItemState
  { title :: JSString
  , completed :: Bool
  , editing :: Maybe JSString
  } deriving stock (Show, Eq, Generic)

data TodoItemAction a where
  CancelAction :: TodoItemConfig -> TodoItemAction ()
  CommitAction :: TodoItemConfig -> TodoItemAction ()
  InputAction :: TodoItemConfig -> JSString -> TodoItemAction ()
  DoubleClickAction :: TodoItemConfig -> JSVal -> TodoItemAction ()
  CheckedAction :: TodoItemConfig -> Bool -> TodoItemAction ()
  KeydownAction :: TodoItemConfig -> Int -> TodoItemAction ()

eval :: TodoItemAction a -> Step a
eval = \case
  CancelAction cfg ->
    modifyRef cfg.state_ref \s -> s{editing=Nothing}
  CommitAction cfg -> do
    state <- readRef cfg.state_ref
    case state.editing of
      Just "" ->
        cfg.ask_delete_item
      Just t ->
        modifyRef cfg.state_ref \s -> s {editing=Nothing, title = t}
      Nothing ->
        pure ()
  InputAction cfg newVal ->
    modifyRef cfg.state_ref \s -> s{editing = Just newVal}
  DoubleClickAction cfg targetEl -> do
    modifyRef cfg.state_ref \s -> s {editing = Just s.title}
    liftIO $ js_todoItemInputFocus targetEl
  CheckedAction cfg isChecked -> do
    modifyRef cfg.state_ref \s -> s{completed = isChecked}
  KeydownAction cfg key -> case key of
    13 {- Enter -} -> eval (CommitAction cfg)
    27 {- Escape -} -> eval (CancelAction cfg)
    _ -> return ()

html :: TodoItemConfig -> Html ()
html cfg = li_ do
  let
    completedDyn = (.completed) <$> fromRef cfg.state_ref
    editingDyn = isJust . (.editing) <$> fromRef cfg.state_ref
    valueDyn = fromMaybe "" . (.editing) <$> fromRef cfg.state_ref
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden_dyn
  div_ [class_ "view"] do
    on "dblclick" $ decodeEvent (propDecoder "target") $
      eval . DoubleClickAction cfg
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.completed) <$> fromRef cfg.state_ref
      on "change" $ decodeEvent checkedDecoder $
        eval . CheckedAction cfg
    label_ $ dynText $ (.title) <$> fromRef cfg.state_ref
    button_ [class_ "destroy"] do
      on_ "click" cfg.ask_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on "input" $ decodeEvent valueDecoder $
      eval . InputAction cfg
    on "keydown" $ decodeEvent keyCodeDecoder $
      eval . KeydownAction cfg
    on_ "blur" $
      eval (CommitAction cfg)

emptyTodoItemState :: TodoItemState
emptyTodoItemState = TodoItemState "" False Nothing

instance ToJSVal TodoItemState where
  toJSVal s = do
    title <- toJSVal s.title
    completed <- toJSVal s.completed
    editing <- toJSVal s.editing
    return $ js_buildObjectI3
      (unJSString "title") title
      (unJSString "completed") completed
      (unJSString "editing") editing

instance FromJSVal TodoItemState where
  fromJSVal j = do
    mtitle <- fromJSVal =<< getProp j "title"
    mcompleted <- fromJSVal =<< getProp j "completed"
    mediting <- fromJSVal =<< getProp j "editing"
    return do
      title <- mtitle
      completed <- mcompleted
      editing <- mediting
      return TodoItemState {..}
