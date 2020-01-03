{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding ((#))
import Data.Text as T
import Data.Text.IO as T
import Massaraksh.Base
import Massaraksh.Main
import Massaraksh.Decode

data Model = Model
  { _moCounter :: Int
  , _moInput   :: Text
  } deriving (Show, Eq)

makeLenses ''Model

data Msg a where
  Init :: Msg Model
  Render :: Msg ()
  Inc :: Msg ()
  Dec :: Msg ()
  Reset :: Msg ()
  Hello :: Msg ()
  Input :: Text -> Msg ()
  GetInput :: Msg Text
  PrintInput :: Msg ()

component
  :: forall m e
   . ( MonadReader e m
     , HasComponent Model Msg e m )
  => Msg
  ~> Producer1 Msg m
component = \case
  Init -> do
    pure (Model (-238742) "initial PlaceHolder")
  Render -> do
    lift render
  Inc -> do
    modify (moCounter %~ (+ 1))
  Dec -> do
    modify (moCounter %~ (+ (-1)))
  Reset -> do
    modify (moInput .~ "")
  Hello -> do
    modify (moInput .~ "Hello, World!!")
    yield1 Inc
  Input txt -> do
    modify (moInput .~ txt)
  GetInput -> do
    gets _moInput
  PrintInput -> do
    txt <- yield1 GetInput
    liftIO (T.putStrLn txt)
  where
    render :: m ()
    render =
      div_ do
        "className" =: "dfsfsd"
        "id"        =: "sdfsd"
        text "Hi"
        ul_ do
          li_ (text "One")
          li_ (text "Two")
          li_ (text "Thre")
          li_ (text "One")
          li_ (text "One")
          li_ (dynText _moInput)
          li_ do
            "className" =: "li"
            dynText (T.pack . show . _moCounter)
        div_ do
          input_ do
            "class" =: "sdfsd"
            "value" ~: _moInput
            on "input" $ Input <$> valueDecoder
        div_ do
          button_ do
            "className" =: "sdfsd"
            on "click" (pure Inc)
            text "Inrement"
          button_ do
            text "Decrement"
            on "click" (pure Dec)
        div_ do
          button_ do
            on "click" (pure Hello)
            text "Say Hello"
          button_ do
            on "click" (pure Reset)
            text "Reset"
        div_ do
          button_ do
            on "click" (pure PrintInput)
            text "Print input"

main = withJSM (Just 3002) $ attachComponentToBody component ()

instance HasRender Msg where
  isRender Render = True
  isRender _      = False
  buildRender = Render

instance HasInit () Model Msg where
  buildInit _ = Init

type (~>) a b = forall x. a x -> b x

type HasComponent s o e m =
  ( MonadWidget e m
  , HasMessage (o ()) e
  , HasModel s e
  , MonadState s m )

newtype Component s o e m = Component
  { unComponent :: HasComponent s o e m => o ~> m }

type HasMessage1 o e m =
  ( HasMessage (o ()) e
  , MonadWidget e m )

type HasModel1 s e m =
  ( HasModel s e
  , MonadState s m )
