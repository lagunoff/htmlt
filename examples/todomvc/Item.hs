{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Item where

import Control.Lens hiding ((#))
import Control.Monad.State
import Data.Maybe
import Data.Default
import Data.Text as T
import GHC.Generics (Generic)
import GHCJS.Marshal
import Language.Javascript.JSaddle
import Component

data Config s = Config
  { cfgModel :: Lens' s Model
  , cfgProps :: s -> Props
  , cfgDynamic :: DynamicRef s
  }

data Props = Props
  { propHidden :: Bool
  } deriving (Show, Eq, Generic)

data Model = Model
  { _moTitle     :: Text
  , _moCompleted :: Bool
  , _moEditing   :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance Default Model where def = Model "" False Nothing

makeLenses ''Model

data Msg a where
  Render :: Msg ()
  Completed :: Bool -> Msg ()
  Destroy :: Msg ()
  Blur :: Msg ()
  EditingOn :: JSVal -> Msg ()
  EditInput :: Text -> Msg ()
  EditingCancel :: Msg ()
  EditingCommit :: Msg ()

itemWidget :: Config s -> Component Msg
itemWidget Config{cfgDynamic = dynRef@(model, modify), ..} yield = \case
  Render -> do
    li_ do
      toggleClass "completed" $ (^. cfgModel . moCompleted) <$> model
      toggleClass "editing" $ (^. cfgModel . moEditing . to isJust) <$> model
      toggleClass "hidden" $ (propHidden . cfgProps) <$> model
      div_ do
        "className" =: "view"
        on "dblclick" $ target <&> yield . EditingOn
        input_ do
          "className" =: "toggle"
          "type"      =: "checkbox"
          "checked"   ~: (^. cfgModel . moCompleted) <$> model
          on "change" $ checked <&> yield . Completed
        label_ $ dynText $ (^. cfgModel . moTitle) <$> model
        button_ do
          "className" =: "destroy"
          on_ "click" do yield Destroy
      input_ do
        "className" =: "edit"
        "type"      =: "text"
        "value"     ~: (^. cfgModel . moEditing . to (fromMaybe "")) <$> model
        on "input" $ value <&> yield . EditInput
        on_ "blur" $ yield Blur
        on "keydown" $ keyCode <&> \case
          13 -> yield EditingCommit -- Enter
          27 -> yield EditingCancel -- Escape
          _  -> pure ()
  Completed x ->
    liftIO $ sync $ modify $ cfgModel . moCompleted .~ x
  Destroy ->
    pure ()
  Blur ->
    yield EditingCommit
  EditingOn elm -> do
    title <- liftIO $ (^. cfgModel . moTitle) <$> dnRead model
    liftIO $ sync $ modify $ cfgModel . moEditing .~ Just title
    void $ liftJSM $ do
      -- FIXME: currentTarget doesn't work for @dblclick@ it gets
      -- assigned to null, @elm@ points to label inside div.view
      input <- pToJSVal elm ! ("parentNode" :: Text) ! ("parentNode" :: Text)
        # ("querySelector" :: Text) $ ["input[type=text]" :: Text]
      cb <- function \_ _ _ -> void $ liftJSM $ input #
        ("focus" :: Text) $ ([] :: [Int])
      jsg2 ("setTimeout" :: Text) cb (100 :: Int)
  EditInput x ->
    liftIO $ sync $ modify $ cfgModel . moEditing %~ fmap (const x)
  EditingCancel -> do
    liftIO $ sync $ modify $ cfgModel . moEditing .~ Nothing
  EditingCommit -> liftIO ((^. cfgModel . moEditing) <$> dnRead model) >>= \case
    Just "" -> yield Destroy
    Just x  -> liftIO $ sync $ modify $ (cfgModel . moEditing .~ Nothing) . (cfgModel . moTitle .~ x)
    Nothing -> pure ()
