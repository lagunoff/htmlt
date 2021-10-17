module Todo.Item where

import Control.Lens hiding ((#))
import Control.Monad.State
import Data.Generics.Labels ()
import Data.Maybe
import Data.Text as T
import GHC.Generics (Generic)
import GHCJS.Marshal
import HtmlT

import "this" Todo.Utils


data TodoItemConfig s = TodoItemConfig
  { tic_dyn_ref :: DynRef s
  , tic_state :: Lens' s ItemState
  , tic_is_hidden :: s -> Bool
  , tic_delete_item :: Html ()
  }

data ItemState = ItemState
  { item_title :: Text
  , item_completed :: Bool
  , item_editing :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSVal, FromJSVal)

todoItemWidget :: TodoItemConfig s -> Html ()
todoItemWidget TodoItemConfig{..} = li_ do
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" hiddenDyn
  div_ [class_ "view"] do
    on "dblclick" $ decodeTarget \targetEl -> do
      title <- readsRef (view (tic_state . #item_title)) tic_dyn_ref
      modifyRef tic_dyn_ref $ tic_state . #item_editing .~ Just title
      liftIO $ js_focus targetEl
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ view (tic_state . #item_completed) <$> fromRef tic_dyn_ref
      on "change" $ decodeChecked \isChecked -> do
        modifyRef tic_dyn_ref $ tic_state . #item_completed .~ isChecked
    label_ $ dynText $ view (tic_state . #item_title) <$> fromRef tic_dyn_ref
    button_ [class_ "destroy"] do
      on_ "click" $ tic_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on "input" $ decodeValue \value -> do
      modifyRef tic_dyn_ref $ tic_state . #item_editing .~ Just value
    on_ "blur" commitEditing
    on "keydown" $ decodeKeyCode \case
      13 -> commitEditing -- Enter
      27 -> cancelEditing -- Escape
      _  -> return ()
  where
    completedDyn = view (tic_state . #item_completed) <$> fromRef tic_dyn_ref
    editingDyn = view (tic_state . #item_editing . to isJust) <$> fromRef tic_dyn_ref
    hiddenDyn = tic_is_hidden <$> fromRef tic_dyn_ref
    valueDyn = view (tic_state . #item_editing . to (fromMaybe "")) <$> fromRef tic_dyn_ref
    commitEditing = readEditing >>= \case
      Just "" -> tic_delete_item
      Just t -> modifyRef tic_dyn_ref
        $ (tic_state . #item_editing .~ Nothing)
        . (tic_state . #item_title .~ t)
      Nothing -> pure ()
      where
        readEditing = readsRef (view (tic_state . #item_editing)) tic_dyn_ref
    cancelEditing = modifyRef tic_dyn_ref $ tic_state . #item_editing .~ Nothing

defaultItemState :: ItemState
defaultItemState = ItemState T.empty False Nothing
