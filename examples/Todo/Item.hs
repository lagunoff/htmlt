{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Todo.Item where

import Control.Lens hiding ((#))
import Control.Monad.State
import Data.Generics.Product
import Data.Maybe
import Data.Text as T
import GHC.Generics (Generic)
import GHCJS.Marshal
import Language.Javascript.JSaddle
import HtmlT

import Todo.Utils

data Config s = Config
  { cfg_state :: Lens' s ItemState
  , cfg_env :: s -> ItemEnv
  , cfg_dyn_ref :: DynRef s
  }

data ItemEnv = ItemEnv
  { item_hidden :: Bool
  } deriving (Show, Eq, Generic)

data ItemState = ItemState
  { item_title :: Text
  , item_completed :: Bool
  , item_editing :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

defaultItemState :: ItemState
defaultItemState = ItemState T.empty False Nothing

data ItemMsg a where
  Render :: ItemMsg ()
  SetCompleted :: Bool -> ItemMsg ()
  Destroy :: ItemMsg ()
  Blur :: ItemMsg ()
  EditingOn :: JSVal -> ItemMsg ()
  EditInput :: Text -> ItemMsg ()
  EditingCancel :: ItemMsg ()
  EditingCommit :: ItemMsg ()

itemWidget :: Config s -> Component ItemMsg
itemWidget Config{..} yield = \case
  Render -> do
    li_ do
      toggleClass "completed" $ (^. cfg_state . to item_completed) <$> fromRef cfg_dyn_ref
      toggleClass "editing" $ (^. cfg_state . to item_editing . to isJust) <$> fromRef cfg_dyn_ref
      toggleClass "hidden" $ item_hidden . cfg_env <$> fromRef cfg_dyn_ref
      div_ [class_ "view"] do
        on "dblclick" $ decodeTarget $ yield . EditingOn
        input_ [class_ "toggle", type_ "checkbox"] do
          dynChecked $ (^. cfg_state . to item_completed) <$> fromRef cfg_dyn_ref
          on "change" $ decodeChecked $ yield . SetCompleted
        label_ $ dynText $ (^. cfg_state . to item_title) <$> fromRef cfg_dyn_ref
        button_ [class_ "destroy"] do
          on_ "click" do yield Destroy
      input_ [class_ "edit", type_ "text"] do
        dynValue $ (^. cfg_state . to item_editing . to (fromMaybe "")) <$> fromRef cfg_dyn_ref
        on "input" $ decodeValue $ yield . EditInput
        on_ "blur" $ yield Blur
        on "keydown" $ decodeKeyCode \case
          13 -> yield EditingCommit -- Enter
          27 -> yield EditingCancel -- Escape
          _  -> pure ()
  SetCompleted x ->
    modifyRef cfg_dyn_ref $ cfg_state . field @"item_completed" .~ x
  Destroy ->
    pure ()
  Blur ->
    yield EditingCommit
  EditingOn elm -> do
    title <- (^. cfg_state . field @"item_title") <$> readRef cfg_dyn_ref
    modifyRef cfg_dyn_ref $ cfg_state . field @"item_editing" .~ Just title
    void $ liftJSM $ do
      -- FIXME: currentTarget doesn't work for @dblclick@ it gets
      -- assigned to null, @elm@ points to label inside div.view
      input <- pToJSVal elm ! ("parentNode" :: Text) ! ("parentNode" :: Text)
        # ("querySelector" :: Text) $ ["input[type=text]" :: Text]
      cb <- function \_ _ _ -> void $ liftJSM $ input #
        ("focus" :: Text) $ ([] :: [Int])
      jsg2 ("setTimeout" :: Text) cb (100 :: Int)
  EditInput x ->
    modifyRef cfg_dyn_ref $ cfg_state . field @"item_editing" .~ Just x
  EditingCancel -> do
    modifyRef cfg_dyn_ref $ cfg_state . field @"item_editing" .~ Nothing
  EditingCommit -> liftIO ((^. cfg_state . field @"item_editing") <$> readRef cfg_dyn_ref) >>= \case
    Just "" -> yield Destroy
    Just x  -> modifyRef cfg_dyn_ref
      $ (cfg_state . field @"item_editing" .~ Nothing)
      . (cfg_state . field @"item_title" .~ x)
    Nothing -> pure ()
