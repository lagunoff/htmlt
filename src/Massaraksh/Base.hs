module Massaraksh.Base where

import Control.Lens (over, Const(..), getConst, Lens, Profunctor(..))
import Massaraksh.Dynamic (Dynamic(..))

-- |Messages emitted by components
data UIMsg widget msg input output
  = Ref { _refWidget :: widget }
  | Step { _stModify :: input -> output }
  | Yield { _yiMessage :: msg }

-- |Represents a chunk of user interface
newtype UI m widget msg input output = UI
  { runUI
      :: Dynamic m input
      -> Sink m (UIMsg widget msg input output)
      -> m (UIHandle m widget) }

-- |Result of running 'runUI'
data UIHandle m widget = UIHandle
  { _uihWidget   :: widget
  , _uihFinalize :: m () }

-- |Callback to consume events of type @a@
type Sink m a = a -> m ()

-- |Map @msg@ inside @UIMsg ui msg i o@
mapMessage :: (msg1 -> msg2) -> UIMsg ui msg1 i o -> UIMsg ui msg2 i o
mapMessage f Ref{..}   = Ref{..}
mapMessage f Step{..}  = Step{..}
mapMessage f Yield{..} = Yield (f _yiMessage)

-- |Map @msg@ inside @UI w m i o@
mapUI :: (a -> b) -> UI e w a i o -> UI e w b i o
mapUI f UI{..} = UI \store sink -> runUI store (sink . mapMessage f)

withInitialModel :: Monad e => (i -> UI e w m i o) -> UI e w m i o
withInitialModel f = UI \store sink -> do
  setup <- runUI . f <$> _dynRead store
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
    mkSetup setup store sink = setup (mkDynamic store) (mkSink sink)
    mkDynamic store = fmap (getConst . stab Const) store
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
