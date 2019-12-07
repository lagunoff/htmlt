module Massaraksh.Dynamic where

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Massaraksh.Event (createEvent, Event(..), EventHandle(..))
import qualified Massaraksh.Event as Event

data Update a = Update
  { _updOld :: a
  , _updNew :: a }

data Dynamic m a = Dynamic
  { _dynRead    :: m a
  , _dynUpdates :: Event m (Update a) }

-- |Result of 'createStore'
data DynamicHandle m a = DynamicHandle
  { _dhDynamic :: Dynamic m a
  , _dhModify  :: (a -> a) -> m () }

-- |Create new 'Store' and a function to update the value inside the
-- store
createDynamic :: MonadIO m => a -> m (DynamicHandle m a)
createDynamic initial = do
  ref <- liftIO $ newIORef initial
  EventHandle event push <- createEvent
  let
    updates = event
    store = Dynamic (liftIO (readIORef ref)) updates
    modify f = do
      old <- liftIO $ readIORef ref
      let new = f old
      liftIO $ writeIORef ref new
      push (Update old new)
  pure (DynamicHandle store modify)

-- |Filter and map
mapMaybe :: Monad m => b -> (a -> Maybe b) -> Dynamic m a -> Dynamic m b
mapMaybe def f Dynamic{..} = Dynamic dynRead dynUpdates
  where
    dynRead = f <$> _dynRead >>= \case
      Just b  -> pure b
      Nothing -> pure def
    dynUpdates = flip Event.mapMaybe (fmap (fmap f) _dynUpdates) \case
      Update (Just old) (Just new) -> Just (Update old new)
      Update _ _                   -> Nothing

instance Functor Update where
  fmap f (Update old new) = Update (f old) (f new)

instance Functor m => Functor (Dynamic m) where
  fmap f Dynamic{..} = Dynamic dynRead dynUpdates
    where
      dynRead = f <$> _dynRead
      dynUpdates = fmap (fmap f) _dynUpdates
