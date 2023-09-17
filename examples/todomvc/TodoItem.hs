module TodoItem where

import Control.Monad.State
import Data.Maybe
import GHC.Generics (Generic)
import GHC.JS.Prim
import HtmlT

import "this" Utils


data TodoItemConfig = TodoItemConfig
  { tic_state_ref :: DynRef TodoItemState
  , tic_is_hidden :: Dynamic Bool
  , tic_delete_item :: Step ()
  }

data TodoItemState = TodoItemState
  { tis_title :: JSString
  , tis_completed :: Bool
  , tis_editing :: Maybe JSString
  } deriving stock (Show, Eq, Generic)

todoItemWidget :: TodoItemConfig -> Html ()
todoItemWidget TodoItemConfig{..} = li_ do
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" tic_is_hidden
  div_ [class_ "view"] do
    on "dblclick" $ decodeEvent (propDecoder "target") \targetEl -> do
      title <- (.tis_title) <$> readRef tic_state_ref
      modifyRef tic_state_ref \s -> s {tis_editing = Just title}
      liftIO $ js_todoItemInputFocus targetEl
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.tis_completed) <$> fromRef tic_state_ref
      on "change" $ decodeEvent checkedDecoder $
        modifyRef tic_state_ref . (\v s -> s{tis_completed = v})
    label_ $ dynText $ (.tis_title) <$> fromRef tic_state_ref
    button_ [class_ "destroy"] do
      on_ "click" $ tic_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on "input" $ decodeEvent valueDecoder $
      modifyRef tic_state_ref . (\v s -> s{tis_editing = v}) . Just
    on "keydown" $ decodeEvent keyCodeDecoder \case
      13 -> commitEditing -- Enter
      27 -> cancelEditing -- Escape
      _ -> return ()
    on_ "blur" commitEditing
  where
    completedDyn =
      (.tis_completed) <$> fromRef tic_state_ref
    editingDyn =
      isJust . (.tis_editing) <$> fromRef tic_state_ref
    valueDyn =
      fromMaybe "" . (.tis_editing) <$> fromRef tic_state_ref
    commitEditing = readEditing >>= \case
      Just "" ->
        tic_delete_item
      Just t ->
        dynStep $ modifyRef tic_state_ref \s -> s
          {tis_editing=Nothing, tis_title = t}
      Nothing ->
        pure ()
      where
        readEditing = (.tis_editing) <$> readRef tic_state_ref
    cancelEditing =
      dynStep $ modifyRef tic_state_ref \s -> s{tis_editing=Nothing}

defaultItemState :: TodoItemState
defaultItemState = TodoItemState "" False Nothing

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
