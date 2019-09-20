{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module Massaraksh where

import Control.Monad.IO.Class
import Data.IORef
import Massaraksh.Event hiding (mapMaybe)
import qualified Massaraksh.Event as Ev

type Sink m a = a -> m ()
data Updates a = Updates { old :: a, new :: a }
data Nested parent model = Nested { parent :: parent, here :: model }

data Store eff a = Store
  { readLatest :: eff a
  , updates    :: Event eff (Updates a)
  }

-- | Predefined messages with
data UIMsg widget msg input output
  = Ref widget
  | Step (input -> output)
  | Yield msg

-- | Represents a chunk of user interface
newtype UI eff widget msg input output = UI
  { unUI
      :: Store eff input
      -> Sink eff (UIMsg widget msg input output)
      -> eff (UIHandle eff widget)
  }

-- | Result of running 'UI'
data UIHandle eff widget = UIHandle
  { uiWidget    :: widget
  , uiFinalizer :: eff ()
  }

-- | Map @msg@ inside @UIMsg ui msg i o@
mapMessage :: (a -> b) -> UIMsg ui a i o -> UIMsg ui b i o
mapMessage f (Ref   ui)  = Ref ui
mapMessage f (Step  io)  = Step io
mapMessage f (Yield msg) = Yield (f msg)

-- | Map @msg@ inside @UI w m i o@
mapUI :: (a -> b) -> UI e w a i o -> UI e w b i o
mapUI f (UI setup) = UI $ \store sink -> setup store (sink . mapMessage f)

-- | Result of 'createStore'
data StoreHandle eff a = StoreHandle
  { shStore       :: Store eff a
  , shModifyStore :: (a -> a) -> eff ()
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
  fmap f (Store rl u) = Store rl' u' where
    rl' = f <$> rl
    u' = (fmap $ fmap f) u

-- | Filter updates from another store
mapMaybe :: MonadIO eff => b -> (a -> Maybe b) -> Store eff a -> Store eff b
mapMaybe def f (Store readA updatesA) = Store {..} where
  readLatest = f <$> readA >>= \case
    Just b -> pure b
    Nothing -> pure def
  updates = flip Ev.mapMaybe updatesA $ \Updates { old, new } -> case (f old, f new) of
    (Just old', Just new') -> Just (Updates old' new')
    (_, _)                 -> Nothing

askModel :: Monad e => (i -> UI e w m i o) -> UI e w m i o
askModel f = UI $ \store sink -> do
  setup <- unUI . f <$> readLatest store
  setup store sink
