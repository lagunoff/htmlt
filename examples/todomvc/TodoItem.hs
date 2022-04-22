module TodoItem where

import Control.Lens hiding ((#))
import Control.Monad.State
import Data.Generics.Labels ()
import Data.Maybe
import Data.Text as T
import GHC.Generics (Generic)
import GHCJS.Marshal
import HtmlT

import "this" Utils


data TodoItemConfig = TodoItemConfig
  { tic_state_ref :: DynRef TodoItemState
  , tic_is_hidden :: Dynamic Bool
  , tic_delete_item :: Transact ()
  }

data TodoItemState = TodoItemState
  { tis_title :: Text
  , tis_completed :: Bool
  , tis_editing :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSVal, FromJSVal)

todoItemWidget :: TodoItemConfig -> Html ()
todoItemWidget TodoItemConfig{..} = li_ do
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" hiddenDyn
  div_ [class_ "view"] do
    onDecoder "dblclick" targetDecoder \targetEl -> do
      title <- readsRef (view #tis_title) tic_state_ref
      modifySync tic_state_ref $ #tis_editing .~ Just title
      liftIO $ js_todoItemInputFocus targetEl
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ view #tis_completed <$> fromRef tic_state_ref
      onDecoder "change" checkedDecoder \isChecked -> do
        modifyRef tic_state_ref $ #tis_completed .~ isChecked
    label_ $ dynText $ view #tis_title <$> fromRef tic_state_ref
    button_ [class_ "destroy"] do
      on_ "click" $ tic_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    onDecoder "input" valueDecoder \value -> do
      modifyRef tic_state_ref $ #tis_editing .~ Just value
    on_ "blur" commitEditing
    onDecoder "keydown" keyCodeDecoder \case
      13 -> commitEditing -- Enter
      27 -> cancelEditing -- Escape
      _  -> return ()
  where
    completedDyn = view #tis_completed <$> fromRef tic_state_ref
    editingDyn = view (#tis_editing . to isJust) <$> fromRef tic_state_ref
    hiddenDyn = tic_is_hidden
    valueDyn = view (#tis_editing . to (fromMaybe "")) <$> fromRef tic_state_ref
    commitEditing = readEditing >>= \case
      Just "" -> tic_delete_item
      Just t -> modifySync tic_state_ref
        $ (#tis_editing .~ Nothing)
        . (#tis_title .~ t)
      Nothing -> pure ()
      where
        readEditing = readsRef (view #tis_editing) tic_state_ref
    cancelEditing = modifySync tic_state_ref $ #tis_editing .~ Nothing

defaultItemState :: TodoItemState
defaultItemState = TodoItemState T.empty False Nothing
