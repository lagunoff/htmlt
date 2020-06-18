module Massaraksh.Event where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List

-- | @Event a@ is a stream of event occurences of type @a@
newtype Event a = Event
  ((a -> IO ()) -> IO (IO ()))

data EventRef a = EventRef
  { eventRef_event   :: Event a
  , eventRef_trigger :: a -> IO ()
  }

data Dyn a = Dyn
  { dyn_read    :: IO a    -- ^ Read current value
  , dyn_updates :: Event a -- ^ Event that fires when the value changes
  }

data DynRef a = DynRef
  { dr_dyn    :: Dyn a
  , dr_modify :: (a -> a) -> IO ()
  }

subscribe :: Event a -> (a -> IO ()) -> IO (IO ())
subscribe (Event s) = s

getEvent :: EventRef a -> Event a
getEvent = eventRef_event

triggerEvent :: EventRef a -> a -> IO ()
triggerEvent = eventRef_trigger

readDyn :: Dyn a -> IO a
readDyn = dyn_read

updates :: Dyn a -> Event a
updates = dyn_updates

getDyn :: DynRef a -> Dyn a
getDyn = dr_dyn

modifyDynRef :: DynRef a -> (a -> a) -> IO ()
modifyDynRef = dr_modify

readDynRef :: DynRef a -> IO a
readDynRef = dyn_read . dr_dyn

dynRefUpdates :: DynRef a -> Event a
dynRefUpdates = dyn_updates . dr_dyn

-- | Create new event and a function to supply values to that event
newEventRef :: IO (EventRef a)
newEventRef = do
  subscribers <- newIORef []
  let
    event = Event \k -> do
      kRef <- newIORef k -- Need 'IORef' for an 'Eq' instance
      modifyIORef subscribers ((:) kRef)
      pure $ modifyIORef subscribers (delete kRef)
    trigger = \a -> do
      callbacks <- readIORef subscribers
      for_ callbacks $ readIORef >=> ($ a)
  pure (EventRef event trigger)

withOld :: a -> Event a -> IO (Event (a, a))
withOld initial e = do
  oldRef <- newIORef initial
  pure $ Event \k -> subscribe e \a -> do
    old <- readIORef oldRef
    writeIORef oldRef a
    k (old, a)

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f e =
  Event \k -> subscribe e $ maybe mempty k . f

-- | Filter and map occurences with side effects
mapMaybeE' :: (a -> IO (Maybe b)) -> Event a -> Event b
mapMaybeE' f e =
  Event \k -> subscribe e \a -> maybe mempty k =<< f a

never :: Event a
never = Event \_ -> mempty

-- | Create new 'Dynamic' and a function to update the value
newDynRef :: a -> IO (DynRef a)
newDynRef initial = do
  ref <- newIORef initial
  e <- newEventRef
  let
    modify = \f -> do
      old <- readIORef ref
      let new = f old
      writeIORef ref new
      triggerEvent e new
  pure $ DynRef (Dyn (readIORef ref) (getEvent e)) modify

mapMaybeD :: b -> (a -> Maybe b) -> Dyn a -> IO (Dyn b)
mapMaybeD def f (Dyn s u) = do
  latestRef <- newIORef def
  let
    read = readIORef latestRef
    updates = flip mapMaybeE' u \upd ->
      case f upd of
        Just new -> do
          writeIORef latestRef new
          pure (Just new)
        Nothing  -> pure Nothing
  pure (Dyn read updates)

constDyn :: a -> Dyn a
constDyn a = Dyn (pure a) never

lensMap :: Lens' s a -> DynRef s -> DynRef a
lensMap stab dRef = DynRef (Dyn read upd) modify where
  read   = fmap (getConst . stab Const) $ readDynRef dRef
  upd    = fmap (getConst . stab Const) $ dynRefUpdates dRef
  modify = \f -> modifyDynRef dRef (over stab f)

holdUniqDynBy :: (a -> a -> Bool) -> Dyn a -> IO (Dyn a)
holdUniqDynBy f = holdUniqDynBy' \a b -> pure (f a b)

holdUniqDynBy' :: (a -> a -> IO Bool) -> Dyn a -> IO (Dyn a)
holdUniqDynBy' f (Dyn r u) = do
  ref <- newIORef =<< r
  let
    updates = flip mapMaybeE' u \new -> do
      old <- readIORef ref
      f old new >>= \case
        True  -> pure Nothing
        False -> Just new <$ writeIORef ref new
  pure (Dyn r updates)

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

instance Functor Dyn where
  fmap f (Dyn s u) = Dyn (fmap f s) (fmap f u)

instance Applicative Dyn where
  pure = constDyn
  (<*>) df da = Dyn s u where
    s = ($) <$> readDyn df <*> readDyn da
    u = Event \k -> do
      c1 <- updates df `subscribe` \f -> do
        new <- f <$> readDyn da
        k new
      c2 <- updates da `subscribe` \a -> do
        new <- ($ a) <$> readDyn df
        k new
      pure (c1 *> c2)

roRef :: DynRef a -> DynRef a
roRef (DynRef (Dyn s _) _) = DynRef (Dyn s never) (\_ -> pure ())

(<**>) :: DynRef a -> DynRef b -> DynRef (a, b)
(<**>) a b = DynRef dyn mod where
  dyn = (,) <$> getDyn a <*> getDyn b
  mod = \f -> do
    oldA <- readDynRef a
    oldB <- readDynRef b
    let (newA, newB) = f (oldA, oldB)
    modifyDynRef a \_ -> newA
    modifyDynRef b \_ -> newB
