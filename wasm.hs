{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Clickable.Html
import Clickable.Internal
import Clickable.Types
import qualified Data.Text as Text
import Clickable.Wasm
import Data.Word
import Foreign (Ptr)
import Data.IORef
import Control.Monad.Reader

test01 :: ClickM ()
test01 = do
  enqueueExpr $ PushStack $ Id "document" `Dot` "body"
  counter <- newVar 0
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
        enqueueExpr $ Call (Id "console") "log" $ valueToExpr val
    el "button" do
      text "Fill the value"
      on @"click" do
        cont <- liftIO $ readIORef ref
        liftIO $ cont $ pure $ Vobj [("this", Vstr "is"), ("some", Vstr "value")]

main :: IO ()
main = pure ()

foreign export ccall wasm_app :: Ptr Word8 -> IO (Ptr Word8)
wasm_app :: Ptr Word8 -> IO (Ptr Word8)
wasm_app = mkWasmApp test01
