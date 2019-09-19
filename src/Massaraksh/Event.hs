module Massaraksh.Event
  ( Event(..)
  , subscribe1
  , createEvent
  , EventHandle(..)
  , mapMaybe
  ) where

import Data.IORef
import Data.List
import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Control.Monad.IO.Class
import GHCJS.DOM.Types (JSM)

-- | Basic subscribe/unsubscribe libary
newtype Event a = Event
  { subscribe :: (a -> JSM ()) -> JSM () -> JSM (JSM ())
  }

instance Functor Event where
  fmap f (Event e) = Event $ \k -> e (k . f)

-- | Same as subscribe but without finalizer
subscribe1 :: Event a -> (a -> JSM ()) -> JSM (JSM ())
subscribe1 (Event e) k = e k $ pure ()

-- | Result for `createEvent`
data EventHandle a = EventHandle
  { getEvent :: Event a
  , pushEvent :: a -> JSM ()
  , finalizeEvent :: JSM ()
  }

-- | Create new `Event a`
createEvent :: JSM (EventHandle a)
createEvent = do
  subscribers <- liftIO $ newIORef []
  let event = Event $ \next complete -> do
        kRef <- liftIO $ newIORef (next, complete)
        liftIO $ modifyIORef subscribers ((:) kRef)
        pure $ liftIO $ modifyIORef subscribers (delete kRef)
      push = \a -> liftIO (readIORef subscribers) >>= traverse_ (liftIO . readIORef >=> \(k, _) -> (k a))
      finish = do
        finalizers <- liftIO $ readIORef subscribers >>= traverse (readIORef >=> pure . snd)
        sequence_ finalizers
        liftIO $ writeIORef subscribers []
  pure $ EventHandle event push finish  

-- | Apply filter
mapMaybe :: (a -> Maybe b) -> Event a -> Event b
mapMaybe f event = Event $ \next finish -> flip (subscribe event) finish $ \a -> maybe (pure ()) next (f a)
