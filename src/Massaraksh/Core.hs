module Massaraksh.Core where

import Control.Lens (over, Const(..), getConst, Lens, Profunctor(..))
import Massaraksh.Store (Store(..))

-- |Messages emitted by components
data UIMsg widget msg input output
  = Ref widget
  | Step (input -> output)
  | Yield msg

-- |Represents a chunk of user interface
newtype UI eff widget msg input output = UI
  { unUI
      :: Store eff input
      -> Sink eff (UIMsg widget msg input output)
      -> eff (UIHandle eff widget)
  }

-- |Result of running 'unUI'
data UIHandle eff widget = UIHandle
  { uiWidget    :: widget
  , uiFinalizer :: eff ()
  }

-- |Callback to consume events of type @a@
type Sink m a = a -> m ()

-- |Map @msg@ inside @UIMsg ui msg i o@
mapMessage :: (msg1 -> msg2) -> UIMsg ui msg1 i o -> UIMsg ui msg2 i o
mapMessage f (Ref ui)    = Ref ui
mapMessage f (Step io)   = Step io
mapMessage f (Yield msg) = Yield (f msg)

-- |Map @msg@ inside @UI w m i o@
mapUI :: (a -> b) -> UI e w a i o -> UI e w b i o
mapUI f (UI setup) = UI \store sink -> setup store (sink . mapMessage f)

withInitialModel :: Monad e => (i -> UI e w m i o) -> UI e w m i o
withInitialModel f = UI \store sink -> do
  setup <- unUI . f <$> readStore store
  setup store sink

-- TODO: Replace @Lens s t a b@ with @Traversal s t a b@
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

instance Functor (UIMsg w m i) where
  fmap f (Ref c)     = Ref c
  fmap f (Step io)   = Step (f . io)
  fmap f (Yield msg) = Yield msg

instance Profunctor (UIMsg w m) where
  dimap g f (Ref c)     = Ref c
  dimap g f (Step io)   = Step (f . io . g)
  dimap g f (Yield msg) = Yield msg

instance Functor e => Profunctor (UI e w m) where
  dimap g f (UI setup) = UI \store sink -> setup (fmap g store) (sink . dimap g f)
