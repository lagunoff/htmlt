{-# LANGUAGE OverloadedStrings #-}
module Main where

import Massaraksh
import Control.Lens
import Control.Monad.IO.Class
import Data.Text as T
import Text.RawString.QQ (r)

data Model = Model
  { _moTab :: Either Text Text
  }

makeLenses ''Model

widget :: HtmlBase m => HtmlT m ()
widget = do
  dynRef <- liftIO $ newDynRef (Model (Left "Left Tab Content"))
  div_ do
    "className" =: "root"
    div_ do
      button_ do
        on_ "click" $ liftIO $ modifyDynRef dynRef $ moTab .~ Left "Left Tab Content"
        "Left"
      button_ do
        on_ "click" $ liftIO $ modifyDynRef dynRef $ moTab .~ Right "Right Tab Content"
        "Right"
    div_ do
      "className" =: "content"
      eitherHtml moTab dynRef leftContent tabContent
    textarea_ do "value" ~: either id id . _moTab <$> getDyn dynRef
    el "style" do "type" =: "text/css"; text css

leftContent :: HtmlBase m => DynRef Text -> HtmlT m ()
leftContent dynRef = do
  div_ do
    "className" =: "tab-content"
    dynRef & textField do
      "className" =: "text-field"
    button_ do
      "Click"
      on_ "click" $ liftIO $ modifyDynRef dynRef (const "Lorem Ipsum")

tabContent :: HtmlBase m => DynRef Text -> HtmlT m ()
tabContent dynRef = do
  div_ do
    "className" =: "tab-content"
    p_ do dynText $ getDyn dynRef

textField :: HtmlBase m => HtmlT m x -> DynRef Text -> HtmlT m x
textField attrs dynRef = input_ do
  "value" ~: getDyn dynRef
  on "input" $ dValue <&> liftIO . modifyDynRef dynRef . const
  attrs

css = [r|
  html, body {
    margin: 0;
    height: 100%;
  }

 .root {
   width: 100%;
   height: 100%;
   text-align: center;
 }
 |]

main = withJSM $ attachToBodySimple widget
