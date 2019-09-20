module Massaraksh.Event where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_, for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (delete)

-- | @Event m a@ is a stream of event occurences of type @a@
newtype Event m a = Event { subscribe :: (a -> m ()) -> m (m ()) }

-- | Result of 'createEvent'
data EventHandle m a = EventHandle
  { ehEvent :: Event m a
  , ehPush  :: a -> m ()
  }

-- | Create new event and a function to supply values to that event
createEvent :: MonadIO m => m (EventHandle m a)
createEvent = do
  subscribers <- liftIO $ newIORef []
  let event = Event $ \k -> do
        kRef <- liftIO $ newIORef k -- Need 'IORef' for an 'Eq' instance
        liftIO $ modifyIORef subscribers ((:) kRef)
        pure $ liftIO $ modifyIORef subscribers (delete kRef)
  let push a = do
        callbacks <- liftIO $ readIORef subscribers
        for_ callbacks $ liftIO . readIORef >=> ($ a)
  pure (EventHandle event push)

-- | Filter and map occurences
mapMaybe :: Applicative m => (a -> Maybe b) -> Event m a -> Event m b
mapMaybe f (Event susbcribeA) = Event subscribeB where
  subscribeB k = susbcribeA $ maybe (pure ()) k . f

instance Functor (Event m) where
  fmap f (Event s) = Event $ s . (. f)

instance MonadIO eff => Applicative (Event eff) where
  pure a = Event $ \k -> k a *> pure (pure ())
  (<*>) e1 e2 = Event $ \k -> do
    latestA <- liftIO $ newIORef Nothing
    latestB <- liftIO $ newIORef Nothing
    c1 <- e1 `subscribe` \a -> do
      liftIO $ writeIORef latestA (Just a)
      liftIO (readIORef latestB) >>= traverse_ (k . a)
    c2 <- e2 `subscribe` \b -> do
      liftIO $ writeIORef latestB (Just b)
      liftIO (readIORef latestA) >>= traverse_ (k . ($ b))
    pure (c1 *> c2)
