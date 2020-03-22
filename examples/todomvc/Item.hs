{-# LANGUAGE ViewPatterns #-}
module Item where

import Control.Lens hiding ((#))
import Control.Monad.State
import Data.Maybe
import Data.Default
import Data.Text as T
import GHC.Generics (Generic)
import GHCJS.Marshal
import GHCJS.Types
import Language.Javascript.JSaddle
import Massaraksh

data Config s = Config
  { cfgModel :: Lens' s Model
  , cfgProps :: s -> Props
  , cfgDynamic :: DynRef s
  }

data Props = Props
  { propHidden :: Bool
  } deriving (Show, Eq, Generic)

data Model = Model
  { _moTitle     :: Text
  , _moCompleted :: Bool
  , _moEditing   :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSVal, FromJSVal)

instance Default Model where
  def = Model "" False Nothing

makeLenses ''Model

data Msg a where
  Render :: Msg ()
  Completed :: Bool -> Msg ()
  Destroy :: Msg ()
  Blur :: Msg ()
  EditingOn :: JSVal -> Msg ()
  EditInput :: Text -> Msg ()
  KeyPress :: Int -> Msg ()
  EditingCancel :: Msg ()
  EditingCommit :: Msg ()

itemWidget :: HtmlBase m => Config s -> HtmlEmit Msg m
itemWidget Config{cfgDynamic = dynRef@(getDyn -> model), ..} yield = \case
  Render -> do
    li_ do
      dynClassList
        [ ("completed", (^. cfgModel . moCompleted) <$> model)
        , ("editing", (^. cfgModel . moEditing . to isJust) <$> model)
        , ("hidden", propHidden . cfgProps <$> model) ]
      div_ do
        "className" =: "view"
        on "dblclick" $ targetDecoder <&> yield . EditingOn
        input_ do
          "className" =: "toggle"
          "type"      =: "checkbox"
          "checked"   ~: (^. cfgModel . moCompleted) <$> model
          on "change" $ checkedDecoder <&> yield . Completed
        label_ $ dynText $ (^. cfgModel . moTitle) <$> model
        button_ do
          "className" =: "destroy"
          on' "click" do yield Destroy
      input_ do
        "className" =: "edit"
        "type"      =: "text"
        "value"     ~: (^. cfgModel . moEditing . to (fromMaybe "")) <$> model
        on "input" $ valueDecoder <&> yield . EditInput
        on' "blur" $ yield Blur
        on "keydown" $ keycodeDecoder <&> yield . KeyPress
  Completed x ->
    liftIO $ modifyDynRef dynRef $ cfgModel . moCompleted .~ x
  Destroy ->
    pure ()
  Blur ->
    yield EditingCommit
  KeyPress code -> do
    if | code == 13 -> yield EditingCommit -- Enter
       | code == 27 -> yield EditingCancel -- Escape
       | otherwise  -> pure ()
  EditingOn elm -> do
    title <- liftIO $ (^. cfgModel . moTitle) <$> readDyn model
    liftIO $ modifyDynRef dynRef $ cfgModel . moEditing .~ Just title
    void $ liftJSM $ do
      -- FIXME: currentTarget doesn't work for @dblclick@ it gets
      -- assigned to null, @elm@ points to label inside div.view
      input <- elm ! ("parentNode" :: Text) ! ("parentNode" :: Text)
        # ("querySelector" :: Text) $ ["input[type=text]" :: Text]
      cb <- function \_ _ _ -> void $ liftJSM $ input #
        ("focus" :: Text) $ ([] :: [Int])
      jsg2 ("setTimeout" :: Text) cb (100 :: Int)
  EditInput x ->
    liftIO $ modifyDynRef dynRef $ cfgModel . moEditing %~ fmap (const x)
  EditingCancel -> do
    liftIO $ modifyDynRef dynRef $ cfgModel . moEditing .~ Nothing
  EditingCommit -> liftIO ((^. cfgModel . moEditing) <$> readDyn model) >>= \case
    Just "" -> yield Destroy
    Just x  -> liftIO $ modifyDynRef dynRef $ (cfgModel . moEditing .~ Nothing) . (cfgModel . moTitle .~ x)
    Nothing -> pure ()
