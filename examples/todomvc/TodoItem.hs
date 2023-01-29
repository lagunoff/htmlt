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
  toggleClass "hidden" tic_is_hidden
  div_ [class_ "view"] do
    on "dblclick" $ decodeEvent (propDecoder "target") \targetEl -> do
      title <- view #tis_title <$> readRef tic_state_ref
      modifyRef tic_state_ref $ set #tis_editing (Just title)
      liftIO $ js_todoItemInputFocus targetEl
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ view #tis_completed <$> fromRef tic_state_ref
      on "change" $ decodeEvent checkedDecoder $
        modifyRef tic_state_ref . set #tis_completed
    label_ $ dynText $ view #tis_title <$> fromRef tic_state_ref
    button_ [class_ "destroy"] do
      on_ "click" $ tic_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on "input" $ decodeEvent valueDecoder $
      modifyRef tic_state_ref . set #tis_editing . Just
    on "keydown" $ decodeEvent keyCodeDecoder \case
      13 -> commitEditing -- Enter
      27 -> cancelEditing -- Escape
      _ -> return ()
    on_ "blur" commitEditing
  where
    completedDyn =
      view #tis_completed <$> fromRef tic_state_ref
    editingDyn =
      view (#tis_editing . to isJust) <$> fromRef tic_state_ref
    valueDyn =
      view (#tis_editing . to (fromMaybe "")) <$> fromRef tic_state_ref
    commitEditing = readEditing >>= \case
      Just "" ->
        tic_delete_item
      Just t ->
        transactionModify tic_state_ref $
          set #tis_editing Nothing . set #tis_title t
      Nothing ->
        pure ()
      where
        readEditing = view #tis_editing <$> readRef tic_state_ref
    cancelEditing =
      transactionModify tic_state_ref $ set #tis_editing Nothing

defaultItemState :: TodoItemState
defaultItemState = TodoItemState T.empty False Nothing
