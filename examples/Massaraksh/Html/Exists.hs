module Massaraksh.Html.Exists
  ( module Massaraksh.Html
  , Exists(..)
  , Html1, Html1'
  , on1
  , on1_
  , onWithOptions1
  , onWithOptions1_
  ) where

import Massaraksh.Html
import GHCJS.DOM.Types
import GHCJS.DOM.EventM (EventName)
import Data.Bifunctor (first)

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
