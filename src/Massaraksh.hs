{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module Massaraksh where

import Control.Monad.IO.Class
import Data.IORef
import GHCJS.DOM.Types (JSM)
import Massaraksh.Event hiding (mapMaybe)
import qualified Massaraksh.Event as Ev

type Sink a = a -> JSM ()
data Updates a = Updates { old :: a, new :: a }
data Nested parent model = Nested { parent :: parent, here :: model }

data Store a = Store
  { readLatest :: JSM a
  , updates    :: Event (Updates a)
  }

-- | Messages components can emit
data UIMsg widget msg input output
  = Ref widget
  | Step (input -> output)
  | Yield msg

-- | Represents a piece of user interface
newtype UI widget msg input output = UI
  { unUI
      :: Store input
      -> Sink (UIMsg widget msg input output)
      -> JSM (UIHandle widget)
  }

-- | Result of running @UI@
data UIHandle widget = UIHandle
  { widget    :: widget
  , finalizer :: JSM ()
  }

-- | Map @msg@ inside @UIMsg ui msg i o@
mapMessage :: (a -> b) -> UIMsg ui a i o -> UIMsg ui b i o
mapMessage f (Ref   ui)  = Ref ui
mapMessage f (Step  io)  = Step io
mapMessage f (Yield msg) = Yield (f msg)

-- | Map @msg@ inside @UI w m i o@
mapUI :: (a -> b) -> UI w a i o -> UI w b i o
mapUI f (UI setup) = UI $ \store sink -> setup store (sink . mapMessage f)

-- | Result of @createStore@
data StoreHandle a = StoreHandle
  { getStore      :: Store a
  , modifyStore   :: (a -> a) -> JSM ()
  , finalizeStore :: JSM ()
  }

-- | Create new @Store a@
createStore :: a -> JSM (StoreHandle a)
createStore initial = do
  ref <- liftIO $ newIORef initial
  EventHandle event push finish <- createEvent
  let updates = event
      store = Store (liftIO (readIORef ref)) updates
      modify f = do
        old <- liftIO $ readIORef ref
        let new = f old
        liftIO $ writeIORef ref new
        push $ Updates old new
  pure $ StoreHandle store modify finish

instance Functor Updates where
  fmap f (Updates old new) = Updates (f old) (f new)

instance Functor Store where
  fmap f (Store readLatest updates) = Store (f <$> readLatest) (fmap (fmap f) updates)

-- | Filter updates from another store
mapMaybe :: (a -> Maybe b) -> Store a -> Store b
mapMaybe f (Store readA updatesA) = Store {..} where
  readLatest = f <$> readA >>= \case
    Just b -> pure b
    Nothing -> error "filterMap: `f` returned Nothing"
  updates = flip Ev.mapMaybe updatesA $ \Updates { old, new } -> case (f old, f new) of
    (Just old', Just new') -> Just (Updates old' new')
    (_, _)                 -> Nothing

askModel :: (i -> UI w m i o) -> UI w m i o
askModel f = UI $ \store sink -> do
  setup <- unUI . f <$> readLatest store
  setup store sink
