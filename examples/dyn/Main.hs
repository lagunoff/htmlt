{-# LANGUAGE OverloadedStrings #-}
module Main where

import Massaraksh
import Control.Lens
import Control.Monad.IO.Class
import Language.Javascript.JSaddle
import Data.Text as T
import Data.Generics.Product
import GHC.Generics
import System.Mem.StableName

data Route
  = IndexR
  | BlogR { page :: Int }
  | AboutR { version :: Text }

indexPage :: HtmlBase m => HtmlT m ()
indexPage = do
  div_ do h1_ do text "Index Page"

blogPage :: HtmlBase m => DynRef ([Text], Int) -> HtmlT m ()
blogPage dynRef = do
  div_ do
    h1_ do "Blog Page"
    ul_ do
      itraverseHtml (_1 . traversed) dynRef \_ txtDyn -> do
        li_ do dynText (getDyn txtDyn)

aboutPage :: HtmlBase m => DynRef Text -> HtmlT m ()
aboutPage dynRef = do
  div_ do
    h1_ do "About the application"
    div_ do dynText $ getDyn dynRef <&> ("Version: " <>)

data Model m = Model
  { route :: !Route
  , page  :: !(HtmlT m ())
  , value :: !Text
  } deriving (Generic)

routeToPage :: HtmlBase m => Route -> HtmlT m ()
routeToPage = \case
  IndexR -> indexPage
  BlogR p -> do
    is <- liftJSM (getBlogItems p)
    dynRef <- liftIO (newDynRef (is, p))
    blogPage dynRef
  AboutR v -> do
    dynRef <- liftIO (newDynRef v)
    aboutPage dynRef

getBlogItems :: Int -> JSM [Text]
getBlogItems _ = do
  pure ["One", "Two", "Three"]

widget :: HtmlBase m => HtmlT m ()
widget = do
  dynRef <- liftIO $ newDynRef (Model IndexR (routeToPage IndexR) "")
  textField (lensMap (field @"value") dynRef) do
    pure ()
  textarea_ do
    "value" ~: (getDyn dynRef <&> getField @"value")
  div_ do
    let
      navigateTo r = do
        let f = \m -> m { route = r, page = routeToPage r }
        liftIO (modifyDynRef dynRef f)
    button_ do "Home"; on_ "click" do navigateTo IndexR
    button_ do "Blog"; on_ "click" do navigateTo (BlogR 1)
    button_ do "About"; on_ "click" do navigateTo (AboutR "param")
  div_ do
    pageDyn <- liftIO $ flip holdUniqDynBy' (getDyn dynRef) \Model{page=a} Model{page=b} -> do
      ptrA <- makeStableName a
      ptrB <- makeStableName b
      pure (ptrA == ptrB)
    dynHtml (pageDyn <&> getField @"page")

textField :: HtmlBase m => DynRef Text -> HtmlT m x -> HtmlT m x
textField dynRef attrs = input_ do
  on "input" $ dValue <&> liftIO . modifyDynRef dynRef . const
  attrs

main = withJSM $ attachToBodySimple widget
