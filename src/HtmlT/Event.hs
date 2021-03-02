module HtmlT.Event where

import Control.Applicative
import Control.Lens (Lens', over)
import Control.Monad.Catch
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.List
import Debug.Trace
import GHC.Generics
import qualified Data.Map as M

import HtmlT.IdSupply

-- | @Event a@ is a stream of event occurences of type @a@
newtype Event a = Event
  { unEvent :: Stage -> Callback a -> IO Canceller
  }
  deriving stock Generic

data Dynamic a = Dynamic
  { dynamic_read :: IO a -- ^ Read current value
  , dynamic_updates :: Event a -- ^ Event that fires when the value changes
  }
  deriving stock Generic

data DynRef a = DynRef
  { dynRef_dynamic :: Dynamic a
  , dynRef_modifier :: Modifier a
  }
  deriving stock Generic

data Stage = Immediate | Defer
  deriving (Show, Eq, Ord, Generic)

newtype ReactiveState = ReactiveState
  { unReactiveState :: M.Map (Id Event) (Reactive ())
  }
  deriving stock Generic

newtype Reactive a = Reactive (StateT ReactiveState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadState ReactiveState,MonadFix, MonadCatch, MonadThrow)
  deriving newtype (MonadMask)

type Callback a = a -> Reactive ()

type Trigger a = a -> Reactive ()

type Modifier a = (a -> a) -> Reactive ()

type Canceller = IO ()

-- | Create new event and a function to supply values to that event
newEvent :: MonadIO m => m (Event a, Trigger a)
newEvent = liftIO do
  immediateSubs <- newIORef []
  deferredSubs <- newIORef []
  eventId <- liftIO $ nextId @Event
  let
    event = Event \fs k -> do
      kRef <- liftIO (newIORef k) -- Need 'IORef' for an 'Eq' instance
      let ref = case fs of Immediate -> immediateSubs; Defer -> deferredSubs
      liftIO $ modifyIORef ref ((:) kRef)
      pure $ liftIO $ modifyIORef ref (delete kRef)
    trigger = \a -> do
      fire a immediateSubs
      defer eventId do fire a deferredSubs
    fire a ref = do
      callbacks <- liftIO (readIORef ref)
      for_ callbacks $ liftIO . readIORef >=> ($ a)
  pure (event, trigger)

-- | Create new 'Dynamic' and a function to update the value
newRef :: MonadIO m => a -> m (DynRef a)
newRef initial = liftIO do
  ref <- newIORef initial
  (ev, push) <- newEvent
  let
    modify = \f -> do
      old <- liftIO (readIORef ref)
      let new = f old
      liftIO (writeIORef ref new)
      push new
    dynamic = Dynamic (readIORef ref) ev
  return $ DynRef dynamic modify

constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

never :: Event a
never = Event \_ -> mempty

writeRef :: MonadIO m => DynRef a -> a -> m ()
writeRef ref a = modifyRef ref (const a)
{-# INLINE writeRef #-}

readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . dynRef_dynamic
{-# INLINE readRef #-}

modifyRef :: MonadIO m => DynRef a -> (a -> a) -> m ()
modifyRef (DynRef _ modifier) = liftIO . sync . modifier
{-# INLINE modifyRef #-}

modifySync :: DynRef a -> (a -> a) -> Reactive ()
modifySync = dynRef_modifier
{-# INLINE modifySync #-}

writeSync :: DynRef a -> a -> Reactive ()
writeSync ref a = modifySync ref (const a)
{-# INLINE writeSync #-}

fromRef :: DynRef a -> Dynamic a
fromRef = dynRef_dynamic
{-# INLINE fromRef #-}

readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . dynamic_read
{-# INLINE readDyn #-}

updates :: Dynamic a -> Event a
updates = dynamic_updates
{-# INLINE updates #-}

defer :: Id Event -> Reactive () -> Reactive ()
defer k act = Reactive (modify f) where
  f (ReactiveState s) = ReactiveState (M.insert k act s)

sync :: Reactive a -> IO a
sync act = do
  let
    loop :: ReactiveState -> Reactive a -> IO a
    loop rs (Reactive act) = do
      (r, newRs) <- runStateT act rs
      case popQueue newRs of
        (Just newAct, newerRs) -> r <$ loop newerRs newAct
        (Nothing, newerRs)     -> return r
  loop (ReactiveState M.empty) act

popQueue :: ReactiveState -> (Maybe (Reactive ()), ReactiveState)
popQueue intact@(ReactiveState m) = case M.minViewWithKey m of
  Just ((_, act), rest) -> (Just act, ReactiveState rest)
  Nothing               -> (Nothing, intact)

subscribe :: Event a -> Callback a -> IO Canceller
subscribe (Event s) = s Defer
{-# INLINE subscribe #-}

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f e = Event \s k -> subscribe e $ maybe mempty k . f
{-# INLINE mapMaybeE #-}

-- | Filter and map occurences with side effects
mapMaybeE' :: (a -> IO (Maybe b)) -> Event a -> Event b
mapMaybeE' f (Event e) = Event \s k ->
  e s \a -> maybe mempty k =<< liftIO (f a)
{-# INLINE mapMaybeE' #-}

mapMaybeD :: MonadIO m => b -> (a -> Maybe b) -> Dynamic a -> IO (Dynamic b)
mapMaybeD def f (Dynamic s u) = do
  latestRef <- newIORef def
  let
    read = readIORef latestRef
    updates = flip mapMaybeE' u \upd ->
      case f upd of
        Just new -> do
          writeIORef latestRef new
          pure (Just new)
        Nothing  -> pure Nothing
  pure (Dynamic read updates)

lensMap :: Lens' s a -> DynRef s -> DynRef a
lensMap stab (DynRef d m) = DynRef (Dynamic read updates) modify where
  read = fmap (getConst . stab Const) $ dynamic_read d
  updates = fmap (getConst . stab Const) $ dynamic_updates d
  modify f = m (over stab f)

holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)

holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equal (Dynamic r (Event s)) = Dynamic r (mapMaybeE id u') where
  u' = Event \x k -> do
    old <- liftIO r
    oldRef <- liftIO (newIORef old)
    s x \new -> do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef new
      k if old `equal` new then Nothing else Just new

holdUniqDynBy' :: (a -> a -> IO Bool) -> Dynamic a -> IO (Dynamic a)
holdUniqDynBy' f (Dynamic r u) = do
  ref <- newIORef =<< r
  let
    updates = flip mapMaybeE' u \new -> do
      old <- readIORef ref
      f old new >>= \case
        True  -> pure Nothing
        False -> Just new <$ writeIORef ref new
  pure (Dynamic r updates)

traceEvent :: Show a => String -> Event a -> Event a
traceEvent = traceEventWith show
{-# INLINE traceEvent #-}

traceEventWith :: (a -> String) -> String -> Event a -> Event a
traceEventWith show' tag (Event f) = Event \s c ->
  f s (c . (\x -> trace (tag ++ ": " ++ show' x) x))
{-# INLINE traceEventWith #-}

traceDyn :: Show a => String -> Dynamic a -> Dynamic a
traceDyn = traceDynWith show
{-# INLINE traceDyn #-}

traceDynWith :: (a -> String) -> String -> Dynamic a -> Dynamic a
traceDynWith show' tag d = d {dynamic_updates = e} where
  e = traceEventWith show' tag (dynamic_updates d)
{-# INLINE traceDynWith #-}

withOld :: a -> Event a -> Event (a, a)
withOld initial (Event s) = Event \x k -> do
  oldRef <- liftIO (newIORef initial)
  s x \a -> do
    old <- liftIO (readIORef oldRef)
    liftIO $ writeIORef oldRef a
    k (old, a)

instance Functor Event where
  fmap f (Event s) = Event \sel k -> s sel . (. f) $ k

instance Semigroup a => Semigroup (Event a) where
  (<>) = liftA2 (<>)

instance Semigroup a => Monoid (Event a) where
  mempty = never

instance Applicative Event where
  pure a = Event \case
    Immediate -> \k ->
      sync $ k a *> mempty
    Defer -> \k -> sync do
      eventId <- liftIO $ nextId @Event
      defer eventId (k a)
      mempty
  (<*>) eF eA = Event \s k -> do
    latestF <- liftIO (newIORef Nothing)
    latestA <- liftIO (newIORef Nothing)
    eventId <- liftIO $ nextId @Event
    let
      doFire = do
        f <- liftIO $ readIORef latestF
        a <- liftIO $ readIORef latestA
        for_ (liftA2 (,) f a) (k . uncurry ($))
      fire = case s of Immediate -> doFire; Defer -> defer eventId doFire
      subscribe (Event s) = s Immediate
    c1 <- eF `subscribe` \f -> do
      liftIO $ writeIORef latestF (Just f)
      fire
    c2 <- eA `subscribe` \a -> do
      liftIO $ writeIORef latestA (Just a)
      fire
    pure (c1 *> c2)

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) df da = Dynamic r u where
    r = liftA2 ($) (dynamic_read df) (dynamic_read da)
    u = Event \s k -> do
      eventId <- liftIO $ nextId @Event
      let
        doFire newF newA = do
          f <- liftIO $ maybe (dynamic_read df) pure newF
          a <- liftIO $ maybe (dynamic_read da) pure newA
          k (f a)
        fire newF newA = case s of
          Immediate -> doFire newF newA
          Defer     -> defer eventId (doFire newF newA)
        subscribe (Event s) = s Immediate
      c1 <- dynamic_updates df `subscribe` \f ->
        fire (Just f) Nothing
      c2 <- dynamic_updates da `subscribe` \a ->
        fire Nothing (Just a)
      pure (c1 *> c2)

instance Semigroup x => Semigroup (Reactive x) where
  (<>) = liftA2 (<>)

instance Monoid x => Monoid (Reactive x) where
  mempty = pure mempty

instance Semigroup ReactiveState where
  (<>) (ReactiveState a) (ReactiveState b) = ReactiveState (a <> b)

instance Monoid ReactiveState where
  mempty = ReactiveState mempty
