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


data TodoItemConfig s = TodoItemConfig
  { tic_ref :: DynRef s
  , tic_state :: Lens' s TodoItemState
  , tic_is_hidden :: s -> Bool
  , tic_delete_item :: Html ()
  }

data TodoItemState = TodoItemState
  { tis_title :: Text
  , tis_completed :: Bool
  , tis_editing :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSVal, FromJSVal)

todoItemWidget :: TodoItemConfig s -> Html ()
todoItemWidget TodoItemConfig{..} = li_ do
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" hiddenDyn
  div_ [class_ "view"] do
    onDecoder "dblclick" targetDecoder \targetEl -> do
      title <- readsRef (view (tic_state . #tis_title)) tic_ref
      modifyRef tic_ref $ tic_state . #tis_editing .~ Just title
      liftIO $ js_focus targetEl
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ view (tic_state . #tis_completed) <$> fromRef tic_ref
      onDecoder "change" checkedDecoder \isChecked -> do
        modifyRef tic_ref $ tic_state . #tis_completed .~ isChecked
    label_ $ dynText $ view (tic_state . #tis_title) <$> fromRef tic_ref
    button_ [class_ "destroy"] do
      on_ "click" $ tic_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    onDecoder "input" valueDecoder \value -> do
      modifyRef tic_ref $ tic_state . #tis_editing .~ Just value
    on_ "blur" commitEditing
    onDecoder "keydown" keyCodeDecoder \case
      13 -> commitEditing -- Enter
      27 -> cancelEditing -- Escape
      _  -> return ()
  where
    completedDyn = view (tic_state . #tis_completed) <$> fromRef tic_ref
    editingDyn = view (tic_state . #tis_editing . to isJust) <$> fromRef tic_ref
    hiddenDyn = tic_is_hidden <$> fromRef tic_ref
    valueDyn = view (tic_state . #tis_editing . to (fromMaybe "")) <$> fromRef tic_ref
    commitEditing = readEditing >>= \case
      Just "" -> tic_delete_item
      Just t -> modifyRef tic_ref
        $ (tic_state . #tis_editing .~ Nothing)
        . (tic_state . #tis_title .~ t)
      Nothing -> pure ()
      where
        readEditing = readsRef (view (tic_state . #tis_editing)) tic_ref
    cancelEditing = modifyRef tic_ref $ tic_state . #tis_editing .~ Nothing

defaultItemState :: TodoItemState
defaultItemState = TodoItemState T.empty False Nothing
