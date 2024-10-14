{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Clickable.Html
import Clickable.Internal
import Clickable.Types
import Clickable.WebSocket
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Network.Wai.Handler.Warp (run)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as Text

test01 :: ClickM ()
test01 = do
  counter <- newVar @Int 0
  attachToBody do
    el "div" do
      prop "className" (Str "container")
      text "Lorem Ipsum"
      el "h1" $ text "Fox jumps over a lazy dog"
      el "p" $ text "Lorem Ipsum sjfh lasfkjh asdl"
      el "button" do
        text "Click Here"
        on @"click" $ modifyVar_ counter succ
      el "button" do
        text "-"
        on @"click" $ modifyVar_ counter pred
      el "br" $ pure ()
      el "span" do
        dynText $ fmap (Text.pack . show) $ fromVar counter
    ref <- liftIO $ newIORef $ const $ pure ()
    el "div" do
      el "button" do
        text "Ask a Value"
        on @"click" do
          t <- asks (.hte_prompt_tag)
          val <- liftIO $ control t \cont -> writeIORef ref cont
          enqueueExpr $ Call (Id "console") "log" [val]
      el "button" do
        text "Fill the value"
        on @"click" do
          cont <- liftIO $ readIORef ref
          liftIO $ cont $ pure $ Obj [("this", Str "is"), ("some", Str "value")]

main :: IO ()
main = do
  run 3000 $ runApp test01

baz :: IO Int
baz = do
  let z :: Int = unsafeCoerce False
  return $ z * 2

bar :: Int -> Int
bar dsfsd = dsfsd
