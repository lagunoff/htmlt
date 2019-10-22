{-# LANGUAGE TemplateHaskell, CPP #-}
module Massaraksh.Component
  ( module Massaraksh.Html
  , Exists(..)
  , Html1
  , Eff
  , on1
  , on1_
  , onWithOptions1
  , onWithOptions1_
  , Emit(..), emit
  , liftMsg
  , interpMsg
  , runStateLens
  , runStateAppHandle
  , io2jsm
  ) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import GHCJS.DOM.EventM (EventName)
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import Language.Javascript.JSaddle (JSM)
import Massaraksh.Html
import Polysemy
import Polysemy.State

-- |Wrapper for messages of kind * -> *
data Exists (f :: * -> *) = forall a. Exists { runExist :: f a }

-- |'Html' with messages of kind * -> *
type Html1 (msg :: * -> *) input output = UI JSM Node (Exists msg) input output

-- |More readable alias for 'Polysemy.Sem'
type Eff e a = forall r. Members e r => Sem r a

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

runStateAppHandle
  :: forall s r a msg
   . Member (Embed JSM) r
  => AppHandle msg s
  -> Sem (State s ': r) a
  -> Sem r a
runStateAppHandle handle = interpret \case
  Get   -> embed $ appHandleReadModel handle
  Put s -> embed $ appHandleStep handle (const s)

runStateLens
  :: forall s r a x
   . Member (State s) r
  => Traversal' s a
  -> Sem (State a ': r) x
  -> Sem r x
runStateLens lens = interpret \case
  Get   -> gets ((!! 0) . (^..lens))
  Put s -> modify (lens .~ s)

io2jsm :: Member (Embed JSM) r => Sem (Embed IO ': r) a -> Sem r a
io2jsm = interpret $ embed @JSM . liftIO . unEmbed  
