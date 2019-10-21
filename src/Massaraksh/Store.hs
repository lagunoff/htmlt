module Massaraksh.Store where

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Massaraksh.Event (createEvent, Event(..), EventHandle(..))
import qualified Massaraksh.Event as Event

data Updates a = Updates { old :: a, new :: a }

data Store eff a = Store
  { readStore :: eff a
  , updates   :: Event eff (Updates a)
  }

-- |Result of 'createStore'
data StoreHandle eff a = StoreHandle
  { getStore    :: Store eff a
  , modifyStore :: (a -> a) -> eff ()
  }

-- |Create new 'Store' and a function to update the value inside the
-- store
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

-- |Filter updates from another store
mapMaybe :: MonadIO eff => b -> (a -> Maybe b) -> Store eff a -> Store eff b
mapMaybe def f (Store latest updates) = Store latest' updates'
  where
    latest' = f <$> latest >>= \case
      Just b  -> pure b
      Nothing -> pure def
    updates' = flip Event.mapMaybe (fmap (fmap f) updates) \case
      Updates (Just old') (Just new') -> Just (Updates old' new')
      Updates _ _                     -> Nothing
  
instance Functor Updates where
  fmap f (Updates old new) = Updates (f old) (f new)

instance Functor eff => Functor (Store eff) where
  fmap f (Store latest updates) = Store latest' updates'
    where
      latest' = f <$> latest
      updates' = fmap (fmap f) updates
