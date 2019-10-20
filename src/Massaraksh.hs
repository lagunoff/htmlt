module Massaraksh where

import Control.Monad.IO.Class
import Data.IORef
import Massaraksh.Event hiding (mapMaybe)
import qualified Massaraksh.Event as Ev

type Sink m a = a -> m ()
data Updates a = Updates { old :: a, new :: a }
data Nested parent model = Nested { parent :: parent, here :: model }

data Store eff a = Store
  { readStore :: eff a
  , updates   :: Event eff (Updates a)
  }

-- |Datatype for messages emitted by 'UI' components
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

-- | Map @msg@ inside @UIMsg ui msg i o@
mapMessage :: (a -> b) -> UIMsg ui a i o -> UIMsg ui b i o
mapMessage f (Ref ui)    = Ref ui
mapMessage f (Step io)   = Step io
mapMessage f (Yield msg) = Yield (f msg)

-- | Map @msg@ inside @UI w m i o@
mapUI :: (a -> b) -> UI e w a i o -> UI e w b i o
mapUI f (UI setup) = UI \store sink -> setup store (sink . mapMessage f)

-- | Result of 'createStore'
data StoreHandle eff a = StoreHandle
  { getStore    :: Store eff a
  , modifyStore :: (a -> a) -> eff ()
  }

-- | Create new 'Store'
createStore :: MonadIO eff => a -> eff (StoreHandle eff a)
createStore initial = do
  ref <- liftIO $ newIORef initial
  EventHandle event push <- createEvent
  let updates = event
  let store = Store (liftIO (readIORef ref)) updates
  let modify f = do
        old <- liftIO $ readIORef ref
        let new = f old
        liftIO $ writeIORef ref new
        push (Updates old new)
  pure (StoreHandle store modify)

instance Functor Updates where
  fmap f (Updates old new) = Updates (f old) (f new)

instance Functor eff => Functor (Store eff) where
  fmap f (Store latest updates) = Store latest' updates'
    where
      latest' = f <$> latest
      updates' = fmap (fmap f) updates

-- | Filter updates from another store
mapMaybe :: MonadIO eff => b -> (a -> Maybe b) -> Store eff a -> Store eff b
mapMaybe def f (Store latest updates) = Store latest' updates'
  where
    latest' = f <$> latest >>= \case
      Just b -> pure b
      Nothing -> pure def
    updates' = flip Ev.mapMaybe updates \Updates { old, new } ->
      case (f old, f new) of
        (Just old', Just new') -> Just (Updates old' new')
        (_, _)                 -> Nothing

askModel :: Monad e => (i -> UI e w m i o) -> UI e w m i o
askModel f = UI \store sink -> do
  setup <- unUI . f <$> readStore store
  setup store sink
