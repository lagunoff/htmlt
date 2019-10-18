{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Massaraksh.Component
  ( module Massaraksh.Html
  , Exists(..)
  , Html1, Html1'
  , on1
  , on1_
  , onWithOptions1
  , onWithOptions1_
  , Component(..)
  , ComponentHandle(..)
  , Emit(..), emit
  , liftMsg
  , interpMsg
  , runStoreState
  , runStateLens
  , io2jsm
  , defaultMain
  ) where

import Language.Javascript.JSaddle (JSM)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import Data.Aeson
import Polysemy
import Polysemy.State
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Massaraksh.Html
import GHCJS.DOM.Types
import GHCJS.DOM.EventM (EventName)
import Data.Bifunctor (first)

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

data Exists f = forall a. Exists { runExist :: f a }

type Html1 msg input output = UI JSM Node (Exists msg) input output
type Html1' msg model = UI JSM Node (Exists msg) model model

on1
  :: IsEvent e
  => EventName HTMLElement e
  -> (i -> Either (msg a) (i -> o))
  -> Attribute (Exists msg) i o
on1 eventName makeMsg = on eventName (first Exists . makeMsg)

on1_
  :: IsEvent e
  => EventName HTMLElement e
  -> msg a
  -> Attribute (Exists msg) i o
on1_ eventName msg = on eventName (const . Left $ Exists msg) 

onWithOptions1
  :: (IsEvent e, ToJSVal e)
  => EventName HTMLElement e
  -> Decoder a
  -> (i -> a -> Either (msg b) (i -> o))
  -> Attribute (Exists msg) i o
onWithOptions1 eventName decoder makeMsg
  = onWithOptions eventName decoder $ (first Exists .) . makeMsg

onWithOptions1_
  :: (IsEvent e, ToJSVal e)
  => EventName HTMLElement e
  -> Decoder a
  -> (a -> msg b)
  -> Attribute (Exists msg) i o
onWithOptions1_ eventName decoder makeMsg
  = onWithOptions eventName decoder (\_ -> Left . Exists . makeMsg)

data Component init eval msg i o = Component
  { init :: init o
  , eval :: forall a. msg a -> eval a
  , view :: Html1 msg i o
  }

data ComponentHandle eff msg i o = ComponentHandle
  { send     :: Either msg (i -> o) -> eff ()
  , widget   :: Node
  , finalize :: eff ()
  }

data Emit msg m a where
  Emit :: msg a -> Emit msg m a
makeSem ''Emit

liftMsg
  :: forall r a msg1 msg2
   . Member (Emit msg2) r
  => (forall b. msg1 b -> msg2 b)
  -> Sem (Emit msg1 ': r) a
  -> Sem r a
liftMsg lift = interpret \(Emit msg) -> emit (lift msg)

interpMsg
  :: forall msg r a
   . (forall b. msg b -> Sem (Emit msg ': r) b)
  -> Sem (Emit msg ': r) a
  -> Sem r a
interpMsg eval = interpret \(Emit msg) -> interpMsg eval (eval msg)

runStoreState :: forall m s r a. Member (Embed m) r => StoreHandle m s -> Sem (State s ': r) a -> Sem r a
runStoreState sh = interpret \case
  Get   -> embed @m $ readStore (shStore sh)
  Put s -> embed @m $ shModifyStore sh (const s)

runStateLens :: forall s r a x. Member (State s) r => Traversal' s a  -> Sem (State a ': r) x -> Sem r x
runStateLens lens = interpret \case
  Get   -> gets ((!! 0) . (^..lens))
  Put s -> modify (lens .~ s)

io2jsm :: Member (Embed JSM) r => Sem (Embed IO ': r) a -> Sem r a
io2jsm = interpret $ embed @JSM . liftIO . unEmbed  

defaultMain
  :: forall r model msg
   . (Member (Embed JSM) r, ToJSON model)
  => JSM model                    -- ^ Init model
  -> (forall a. msg a -> Sem (Emit msg ': State model ': r) a)  -- ^ Components' eval function
  -> Html1' msg model             -- ^ Components' view
  -> (forall a. Sem r a -> JSM a) -- ^ Evaluate the rest of effects
  -> IO ()
defaultMain init eval view runSem = do
  let mainClient = do
        doc <- currentDocumentUnchecked
        body <- getBodyUnchecked doc
        storeHandle <- createStore =<< init
        let sink (Yield (Exists msg)) = do
              _ <- eval msg
                & interpMsg eval
                & runStoreState storeHandle
                & runSem
              pure ()
            sink (Step io) = shModifyStore storeHandle io
            sink (Ref _) = pure ()
        UIHandle node _ <- unUI view (shStore storeHandle) sink
        appendChild_ body node
#ifdef ghcjs_HOST_OS
  mainClient
#else
  portOrEx <- try @SomeException (read <$> getEnv "PORT")
  progName <- getProgName
  let port = either (const 8080) id portOrEx
  let runWarp = if progName == "<interactive>" then Warp.debug else Warp.run
  runWarp port mainClient
#endif
