module Massaraksh.Lens where

import Control.Lens
import Massaraksh

focusN
  :: forall e w m s t a b p
   . Functor e
  => Lens s t a b
  -> UI e w m (Nested p a) (Nested p b)
  -> UI e w m (Nested p s) (Nested p t)
focusN stab (UI setup1) = UI (mkSetup setup1)
  where
    mkSetup setup store sink = setup (mkStore store) (mkSink sink)
    mkStore store = fmap (\s -> getConst ((inHere stab) Const s)) store
    mkSink sink (Ref c)     = sink $ Ref c
    mkSink sink (Step io)   = sink $ Step (over (inHere stab) io)
    mkSink sink (Yield msg) = sink $ Yield msg

focus
  :: forall e w m s t a b
   . Functor e
  => Lens s t a b
  -> UI e w m a b
  -> UI e w m s t
focus stab (UI setup1) = UI (mkSetup setup1)
  where
    mkSetup setup store sink = setup (mkStore store) (mkSink sink)
    mkStore store = fmap (getConst . stab Const) store
    mkSink sink (Ref c)     = sink $ Ref c
    mkSink sink (Step io)   = sink $ Step (over stab io)
    mkSink sink (Yield msg) = sink $ Yield msg

inHere
  :: forall s t a b p
   . Lens s t a b
  -> Lens (Nested p s) (Nested p t) (Nested p a) (Nested p b)
inHere stab = lens getter setter
  where
    getter (Nested p s) = Nested p (getConst (stab Const s))
    setter (Nested _ s) (Nested p b) = Nested p (s & stab .~ b)

nestedId :: forall s. Lens' s (Nested s s)
nestedId = lens getter setter
  where
    getter s = Nested s s
    setter t (Nested _ s) = s

instance Profunctor (UIMsg w m) where
  dimap g f (Ref c)     = Ref c
  dimap g f (Step io)   = Step (f . io . g)
  dimap g f (Yield msg) = Yield msg
