module Massaraksh.Event where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List

-- | @Event m a@ is a stream of event occurences of type @a@
newtype Event a = Event
  { eventSubscribe :: (a -> IO ()) -> IO (IO ()) }

subscribe :: Event a -> (a -> IO ()) -> IO (IO ())
subscribe = eventSubscribe

data EventRef a = EventRef
  { erefValue :: Event a
  , erefPush  :: a -> IO () }

-- | Create new event and a function to supply values to that event
newEventRef :: IO (EventRef a)
newEventRef = do
  subscribers <- newIORef []
  let
    erefValue = Event \k -> do
      kRef <- newIORef k -- Need 'IORef' for an 'Eq' instance
      modifyIORef subscribers ((:) kRef)
      pure $ modifyIORef subscribers (delete kRef)
    erefPush a = do
      callbacks <- readIORef subscribers
      for_ callbacks $ readIORef >=> ($ a)
  pure EventRef{..}

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f Event{..} =
  Event \k -> eventSubscribe $ maybe mempty k . f

-- | Filter and map occurences with side effects
mapMaybeIOE :: (a -> IO (Maybe b)) -> Event a -> Event b
mapMaybeIOE f Event{..} =
  Event \k -> eventSubscribe \a -> maybe mempty k =<< f a

never :: Event a
never = Event \_ -> mempty

instance Functor Event where
  fmap f (Event s) = Event $ s . (. f)

instance Semigroup a => Semigroup (Event a) where
  (<>) = liftA2 (<>)

instance Semigroup a => Monoid (Event a) where
  mempty = never

instance Applicative Event where
  pure a = Event \k -> k a *> mempty
  (<*>) e1 e2 = Event \k -> do
    latestA <- newIORef Nothing
    latestB <- newIORef Nothing
    c1 <- e1 `subscribe` \a -> do
      writeIORef latestA (Just a)
      readIORef latestB >>= traverse_ (k . a)
    c2 <- e2 `subscribe` \b -> do
      writeIORef latestB (Just b)
      readIORef latestA >>= traverse_ (k . ($ b))
    pure (c1 *> c2)
