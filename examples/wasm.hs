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

import Clickable
import qualified Data.Text as Text
import Clickable.WASM
import Data.Word
import Foreign (Ptr)
import Data.IORef
import Control.Monad.Reader

test01 :: JSM ()
test01 = execHTMLBody do
  counter <- liftJSM $ newVar 0
  el "div" do
    property "className" (Str "container")
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
        t <- asks (.ien_prompt_tag)
        val <- liftIO $ control t \cont -> writeIORef ref cont
        jsCmd $ Call (Id "console") "log" [val]
    el "button" do
      text "Fill the value"
      on @"click" do
        cont <- liftIO $ readIORef ref
        liftIO $ cont $ pure $ Obj [("this", Str "is"), ("some", Str "value")]

main :: IO ()
main = pure ()

foreign export ccall wasm_app :: Ptr Word8 -> IO (Ptr Word8)
wasm_app :: Ptr Word8 -> IO (Ptr Word8)
wasm_app = mkWasmApp test01
