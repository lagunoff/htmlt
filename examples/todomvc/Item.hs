module Item where

import Control.Lens hiding ((#))
import Control.Monad.State
import Data.Maybe
import Data.Text as T
import GHC.Generics (Generic)
import GHCJS.Types
import GHCJS.Marshal
import Massaraksh
import Language.Javascript.JSaddle

data Config s = Config
  { cfgProps :: s -> Props
  , cfgModel :: Lens' s Model }

data Props = Props
  { propHidden :: Bool
  } deriving (Show, Eq, Generic)

data Model = Model
  { _moTitle     :: Text
  , _moCompleted :: Bool
  , _moEditing   :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSVal, FromJSVal)

makeLenses ''Model

data Msg a where
  Init :: Text -> Msg Model
  Render :: Msg ()
  Completed :: Bool -> Msg ()
  Destroy :: Msg ()
  Blur :: Msg ()
  EditingOn :: JSVal -> Msg ()
  EditInput :: Text -> Msg ()
  KeyPress :: Int -> Msg ()
  EditingCancel :: Msg ()
  EditingCommit :: Msg ()

itemWidget :: HtmlBase m => Config s -> HtmlRec Msg s m
itemWidget Config{..} yield = \case
  Init title ->
    pure (Model title False Nothing)
  Render -> do
    li_ do
      dynClassList
        [ ("completed", (^. cfgModel . moCompleted))
        , ("editing", (^. cfgModel . moEditing . to isJust))
        , ("hidden", propHidden . cfgProps) ]
      div_ do
        "className" =: "view"
        on "dblclick" $ targetDecoder <&> yield . EditingOn
        input_ do
          "className" =: "toggle"
          "type"      =: "checkbox"
          "checked"   ~: (^. cfgModel . moCompleted)
          on "change" $ checkedDecoder <&> yield . Completed
        label_ $ dynText (^. cfgModel . moTitle)
        button_ do
          "className" =: "destroy"
          on' "click" do yield Destroy
      input_ do
        "className" =: "edit"
        "type"      =: "text"
        "value"     ~: (^. cfgModel . moEditing . to (fromMaybe ""))
        on "input" $ valueDecoder <&> yield . EditInput
        on' "blur" $ yield Blur
        on "keydown" $ keycodeDecoder <&> yield . KeyPress
  Completed x ->
    modify $ cfgModel . moCompleted .~ x
  Destroy ->
    pure ()
  Blur ->
    yield EditingCommit
  KeyPress code -> do
    if | code == 13 -> yield EditingCommit -- Enter
       | code == 27 -> yield EditingCancel -- Escape
       | otherwise  -> pure ()
  EditingOn elm -> do
    title <- gets (^. cfgModel . moTitle)
    modify $ cfgModel . moEditing .~ Just title
    void $ liftJSM $ do
      -- FIXME: currentTarget doesn't work for @dblclick@ it gets
      -- assigned to null, @elm@ points to label inside div.view
      input <- elm ! ("parentNode" :: Text) ! ("parentNode" :: Text)
        # ("querySelector" :: Text) $ ["input[type=text]" :: Text]
      cb <- function \_ _ _ -> void $ liftJSM $ input #
        ("focus" :: Text) $ ([] :: [Int])
      jsg2 ("setTimeout" :: Text) cb (100 :: Int)
  EditInput x ->
    modify $ cfgModel . moEditing %~ fmap (const x)
  EditingCancel -> do
    modify $ cfgModel . moEditing .~ Nothing
  EditingCommit -> gets (^. cfgModel . moEditing) >>= \case
    Just "" -> yield Destroy
    Just x  -> modify $ (cfgModel . moEditing .~ Nothing) . (cfgModel . moTitle .~ x)
    Nothing -> pure ()
