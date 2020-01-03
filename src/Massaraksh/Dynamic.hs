module Massaraksh.Dynamic where

import Data.IORef (newIORef, readIORef, writeIORef)
import Massaraksh.Event (newEvent, Event(..), EventHandle(..), mapMaybeE)

data Update a = Update
  { _updOld :: a
  , _updNew :: a }

data Dynamic a = Dynamic
  { _dynRead    :: IO a
  , _dynUpdates :: Event (Update a) }

-- |Result of 'newDynamic'
data DynamicHandle a = DynamicHandle
  { _dhDynamic :: Dynamic a
  , _dhModify  :: (a -> a) -> IO () }

-- |Create new 'Store' and a function to update the value inside the
-- store
newDynamic :: a -> IO (DynamicHandle a)
newDynamic initial = do
  ref <- newIORef initial
  EventHandle{..} <- newEvent
  let
    _dhDynamic = Dynamic (readIORef ref) _evhEvent
    _dhModify f = do
      old <- readIORef ref
      let new = f old
      writeIORef ref new
      _evhPush (Update old new)
  pure DynamicHandle{..}

-- |Filter and map
mapMaybeD :: b -> (a -> Maybe b) -> Dynamic a -> Dynamic b
mapMaybeD def f Dynamic{..} = Dynamic dynRead dynUpdates
  where
    dynRead = f <$> _dynRead >>= \case
      Just b  -> pure b
      Nothing -> pure def
    dynUpdates = flip mapMaybeE (fmap (fmap f) _dynUpdates) \case
      Update (Just old) (Just new) -> Just (Update old new)
      Update _ _                   -> Nothing

instance Functor Update where
  fmap f (Update old new) = Update (f old) (f new)

instance Functor Dynamic where
  fmap f Dynamic{..} = Dynamic dynRead dynUpdates
    where
      dynRead = f <$> _dynRead
      dynUpdates = fmap (fmap f) _dynUpdates
