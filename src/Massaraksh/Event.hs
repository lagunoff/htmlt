module Massaraksh.Event where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List

-- |@Event m a@ is a stream of event occurences of type @a@
newtype Event a = Event
  { _evSubscribe :: (a -> IO ()) -> IO (IO ()) }

-- |The result of 'newEvent'
data EventHandle a = EventHandle
  { _evhEvent :: Event a
  , _evhPush  :: a -> IO () }

-- |Create new event and a function to supply values to that event
newEvent :: IO (EventHandle a)
newEvent = do
  subscribers <- newIORef []
  let
    _evhEvent = Event \k -> do
      kRef <- newIORef k -- Need 'IORef' for an 'Eq' instance
      modifyIORef subscribers ((:) kRef)
      pure $ modifyIORef subscribers (delete kRef)
    _evhPush a = do
      callbacks <- readIORef subscribers
      for_ callbacks $ readIORef >=> ($ a)
  pure EventHandle{..}

never :: Event a
never = Event \_ -> mempty

-- |Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f Event{..} = Event subscribe
  where
    subscribe k = _evSubscribe $ maybe mempty k . f

instance Functor Event where
  fmap f (Event s) = Event $ s . (. f)

instance Applicative Event where
  pure a = Event \k -> k a *> mempty
  (<*>) e1 e2 = Event \k -> do
    latestA <- newIORef Nothing
    latestB <- newIORef Nothing
    c1 <- e1 `_evSubscribe` \a -> do
      writeIORef latestA (Just a)
      readIORef latestB >>= traverse_ (k . a)
    c2 <- e2 `_evSubscribe` \b -> do
      writeIORef latestB (Just b)
      readIORef latestA >>= traverse_ (k . ($ b))
    pure (c1 *> c2)
