module TodoItem where

import Control.Monad.State
import Data.Maybe
import GHC.Generics (Generic)
import GHC.JS.Prim
import HtmlT
import JavaScript.Compat.Marshal
import JavaScript.Compat.String (JSString(..))

import "this" Utils

data TodoItemConfig = TodoItemConfig
  { state_ref :: DynRef TodoItemState
  , is_hidden :: Dynamic Bool
  , ask_delete_item :: Step ()
  }

data TodoItemState = TodoItemState
  { tis_title :: JSString
  , tis_completed :: Bool
  , tis_editing :: Maybe JSString
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
    modifyRef cfg.state_ref \s -> s{tis_editing=Nothing}
  CommitAction cfg -> do
    state <- readRef cfg.state_ref
    case state.tis_editing of
      Just "" ->
        cfg.ask_delete_item
      Just t ->
        modifyRef cfg.state_ref \s -> s {tis_editing=Nothing, tis_title = t}
      Nothing ->
        pure ()
  InputAction cfg newVal ->
    modifyRef cfg.state_ref \s -> s{tis_editing = Just newVal}
  DoubleClickAction cfg targetEl -> do
    modifyRef cfg.state_ref \s -> s {tis_editing = Just s.tis_title}
    liftIO $ js_todoItemInputFocus targetEl
  CheckedAction cfg isChecked -> do
    modifyRef cfg.state_ref \s -> s{tis_completed = isChecked}
  KeydownAction cfg key -> case key of
    13 {- Enter -} -> eval (CommitAction cfg)
    27 {- Escape -} -> eval (CancelAction cfg)
    _ -> return ()

html :: TodoItemConfig -> Html ()
html cfg = li_ do
  let
    completedDyn =
      (.tis_completed) <$> fromRef cfg.state_ref
    editingDyn =
      isJust . (.tis_editing) <$> fromRef cfg.state_ref
    valueDyn =
      fromMaybe "" . (.tis_editing) <$> fromRef cfg.state_ref
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden
  div_ [class_ "view"] do
    on "dblclick" $ decodeEvent (propDecoder "target") $
      eval . DoubleClickAction cfg
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.tis_completed) <$> fromRef cfg.state_ref
      on "change" $ decodeEvent checkedDecoder $
        eval . CheckedAction cfg
    label_ $ dynText $ (.tis_title) <$> fromRef cfg.state_ref
    button_ [class_ "destroy"] do
      on_ "click" $ cfg.ask_delete_item
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
    tis_title <- toJSVal s.tis_title
    tis_completed <- toJSVal s.tis_completed
    tis_editing <- toJSVal s.tis_editing
    return $ js_buildObjectI3
      (unJSString "tis_title") tis_title
      (unJSString "tis_completed") tis_completed
      (unJSString "tis_editing") tis_editing

instance FromJSVal TodoItemState where
  fromJSVal j = do
    m_tis_title <- fromJSVal =<< getProp j "tis_title"
    m_tis_completed <- fromJSVal =<< getProp j "tis_completed"
    m_tis_editing <- fromJSVal =<< getProp j "tis_editing"
    return do
      tis_title <- m_tis_title
      tis_completed <- m_tis_completed
      tis_editing <- m_tis_editing
      return TodoItemState {..}
