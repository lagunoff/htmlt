-- | Very simplistic implementation of FRP-like functionality. Most
-- notable definiions are 'Event', 'Dynamic' and 'DynRef'. 'Event' and
-- 'Dynamic' have the same meaning as in reflex. 'DynRef' is a new
-- construction it has similar API to 'IORef' and can be thought of as
-- a 'Dynamic' with a function to update its value
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
import System.IO.Unsafe
import GHC.Exts

import HtmlT.IdSupply

-- | Stream of event occurences of type @a@. Actual representation is
-- just a function that subscribes to the event and returns the action
-- to unsubscribe.
newtype Event a = Event
  { unEvent :: Stage -> Callback a -> IO Canceller
  }
  deriving stock Generic

-- | Holds a value that changes over the lifetime of the
-- program. It is possible to read the current value of a 'Dynamic'
-- and subscribe to its future changes.
data Dynamic a = Dynamic
  { dynamic_read :: IO a
  -- ^ Read current value, use its public alias 'readDyn' instead
  , dynamic_updates :: Event a
  -- ^ Event that fires when the value changes, use its public alias
  -- 'updates' instead
  }
  deriving stock Generic

-- | Mutable variable that supports subscription to its changes. Has
-- similar API to 'IORef' (see 'readRef', 'writeRef', 'modifyRef')
data DynRef a = DynRef
  { dr_dynamic :: Dynamic a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , dr_modifier :: Modifier a
  -- ^ Funtion to update the value
  }
  deriving stock Generic

-- | Used as an argument to internal subscribe function. If caller
-- subscribes to an event using 'Immediate' it will receive all
-- (possibly more than one) intermediate notifications throughout
-- execution of 'sync'. If 'Defer' is used then listener will be only
-- notified once
data Stage = Immediate | Defer
  deriving (Show, Eq, Ord, Generic)

-- | State inside 'Reactive'
newtype ReactiveState = ReactiveState
  { unReactiveState :: M.Map (Id Event) (Reactive ())
  }
  deriving stock Generic

-- | Evaluation of effects triggered by firing of an event
newtype Reactive a = Reactive (StateT ReactiveState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadState ReactiveState,MonadFix, MonadCatch, MonadThrow)
  deriving newtype (MonadMask)

type Callback a = a -> Reactive ()

type Trigger a = a -> Reactive ()

type Modifier a = (a -> a) -> Reactive ()

type Canceller = IO ()

-- | Create new event and a function to supply values to that event
--
-- > (event, push) <- newEvent @String
-- > push "New Value" -- event fires with given value
newEvent :: forall a m. MonadIO m => m (Event a, Trigger a)
newEvent = liftIO do
  immediateSubs <- newIORef []
  deferredSubs <- newIORef []
  eventId <- liftIO $ nextId @Event
  let
    event = Event \fs k -> do
      kRef <- liftIO (newIORef k) -- Need 'IORef' for an 'Eq' instance
      let ref = case fs of Immediate -> immediateSubs; Defer -> deferredSubs
      liftIO $ modifyIORef' ref ((:) kRef)
      pure $ liftIO $ modifyIORef' ref (delete kRef)
    trigger = \a -> do
      fire a immediateSubs
      defer eventId do fire a deferredSubs
    fire a ref = do
      callbacks <- liftIO (readIORef ref)
      for_ callbacks $ liftIO . readIORef >=> ($ a)
  pure (event, trigger)

-- | Create new 'DynRef' using given initial value
--
-- > showRef <- newRef False
-- > writeRef showRef True -- update event fires for showRef
newRef :: forall a m. MonadIO m => a -> m (DynRef a)
newRef initial = liftIO do
  ref <- newIORef initial
  (ev, push) <- newEvent
  let
    modify f = do
      old <- liftIO (readIORef ref)
      let new = f old
      liftIO (writeIORef ref new)
      push new
    dynamic = Dynamic (readIORef ref) ev
  return $ DynRef dynamic modify

-- | Create a Dynamic that never changes its value
constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

-- | Event that will never fire
never :: Event a
never = Event \_ -> mempty

-- | Write new value into a 'DynRef'
--
-- > ref <- newRef "Initial value"
-- > writeRef ref "New value"
-- > readRef ref
-- "New value"
writeRef :: MonadIO m => DynRef a -> a -> m ()
writeRef ref a = modifyRef ref (const a)
{-# INLINE writeRef #-}

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . dr_dynamic
{-# INLINE readRef #-}

-- | Same as 'readRef' but also applies given function to the value
-- held by 'DynRef'
readsRef :: MonadIO m => (a -> b) -> DynRef a -> m b
readsRef f = readsDyn f . dr_dynamic
{-# INLINE readsRef #-}

-- | Update a 'DynRef' by applying a given function to the current
-- value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: MonadIO m => DynRef a -> (a -> a) -> m ()
modifyRef (DynRef _ modifier) = liftIO . sync . modifier
{-# INLINE modifyRef #-}

-- | Version of 'modifyRef' that runs inside @Reactive@
modifySync :: DynRef a -> (a -> a) -> Reactive ()
modifySync = dr_modifier
{-# INLINE modifySync #-}

-- | Version of 'writeRef' that runs inside @Reactive@
writeSync :: DynRef a -> a -> Reactive ()
writeSync ref a = modifySync ref (const a)
{-# INLINE writeSync #-}

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = dr_dynamic
{-# INLINE fromRef #-}

-- | Read the value held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . dynamic_read
{-# INLINE readDyn #-}

-- | Same as 'readDyn' but also applies given function to the value
-- held by 'Dynamic'
readsDyn :: MonadIO m => (a -> b) -> Dynamic a -> m b
readsDyn f = fmap f . liftIO . dynamic_read
{-# INLINE readsDyn #-}

-- | Extract the updates Event from a 'Dynamic'
updates :: Dynamic a -> Event a
updates = dynamic_updates
{-# INLINE updates #-}

-- | Defer a computation (usually an event firing) till the end of
-- current reactive transaction. This makes possible to avoid double
-- firing of events, constructed from multiple other events
defer :: Id Event -> Reactive () -> Reactive ()
defer k act = Reactive (modify f) where
  f (ReactiveState s) = ReactiveState (M.insert k act s)

-- | Run a reactive transaction
sync :: MonadIO m => Reactive a -> m a
sync act = liftIO $ loop (ReactiveState M.empty) act where
  loop :: ReactiveState -> Reactive a -> IO a
  loop rs (Reactive act) = do
    (r, newRs) <- runStateT act rs
    case popQueue newRs of
      (Just newAct, newerRs) -> r <$ loop newerRs newAct
      (Nothing, newerRs)     -> return r
  popQueue intact@(ReactiveState m) = case M.minViewWithKey m of
    Just ((_, act), rest) -> (Just act, ReactiveState rest)
    Nothing               -> (Nothing, intact)

-- | Attach a listener to an event and return a function that removes
-- this listener
subscribe :: Event a -> Callback a -> IO Canceller
subscribe (Event s) = s Defer
{-# INLINE subscribe #-}

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f e = Event \s k -> subscribe e $ maybe mempty k . f
{-# INLINE mapMaybeE #-}

-- | Apply a lens to the value inside 'DynRef'
lensMap :: Lens' s a -> DynRef s -> DynRef a
lensMap stab (DynRef d m) = DynRef (Dynamic read updates) modify where
  read = fmap (getConst . stab Const) $ dynamic_read d
  updates = fmap (getConst . stab Const) $ dynamic_updates d
  modify f = m (over stab f)

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equal Dynamic{..} = newDyn where
  upd = Event \x k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates x \new -> do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef new
      k if old `equal` new then Nothing else Just new
  newDyn = Dynamic dynamic_read (mapMaybeE id upd)

-- | Print a debug message when given event fires
traceEvent :: Show a => String -> Event a -> Event a
traceEvent = traceEventWith show
{-# INLINE traceEvent #-}

-- | Print a debug message when the event fires using given printing
-- function
traceEventWith :: (a -> String) -> String -> Event a -> Event a
traceEventWith show' tag (Event f) = Event \s c ->
  f s (c . (\x -> if s == Defer then trace (tag ++ ": " ++ show' x) x else x))
{-# INLINE traceEventWith #-}

-- | Print a debug message when value inside Dynamic changes
traceDyn :: Show a => String -> Dynamic a -> Dynamic a
traceDyn = traceDynWith show
{-# INLINE traceDyn #-}

-- | Print a debug message when value inside Dynamic changes
traceRef :: Show a => String -> DynRef a -> DynRef a
traceRef s DynRef{..} = DynRef{dr_dynamic = traceDynWith show s dr_dynamic, ..}

-- | Print a debug message when value inside Dynamic changes using
-- given printing function
traceDynWith :: (a -> String) -> String -> Dynamic a -> Dynamic a
traceDynWith show' tag d = d {dynamic_updates = e} where
  e = traceEventWith show' tag (dynamic_updates d)
{-# INLINE traceDynWith #-}

-- | Make an event that contains a tuple with previous value as well
-- as the current value from the given event
{-# DEPRECATED #-}
withOld :: a -> Event a -> Event (a, a)
withOld initial (Event s) = Event \x k -> do
  oldRef <- liftIO (newIORef initial)
  let
    f a = do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef a
      k (old, a)
  s x f

fmapHold :: MonadIO m => (a -> b) -> Dynamic a -> m (Dynamic b)
fmapHold f Dynamic{..} = liftIO do
  initialA <- dynamic_read
  bRef <- newIORef (f initialA)
  eventId <- liftIO $ nextId @Event
  (updates, trigUpdates) <- newEvent
  -- FIXME: Memory leak!
  unEvent dynamic_updates Defer \newA -> do
    let newB = f newA
    bRef <- liftIO $ writeIORef bRef newB
    trigUpdates newB
  return $ Dynamic (readIORef bRef) updates

splatIO :: MonadIO m => Dynamic (a -> b) -> Dynamic a -> m (Dynamic b)
splatIO df da = liftIO do
  fRef <- newIORef =<< dynamic_read df
  aRef <- newIORef =<< dynamic_read da
  bRef <- newIORef =<< liftA2 ($) (readIORef fRef) (readIORef aRef)
  let
    r = readIORef bRef
    u = Event \s k -> do
      eventId <- liftIO $ nextId @Event
      let
        doFire = do
          f <- liftIO $ readIORef fRef
          a <- liftIO $ readIORef aRef
          let newB = f a
          liftIO $ writeIORef bRef newB
          k newB
        fire = case s of
          Immediate -> doFire
          Defer     -> defer eventId doFire
      c1 <- unEvent (dynamic_updates df) s \f -> do
        liftIO $ writeIORef fRef f
        fire
      c2 <- unEvent (dynamic_updates da) s \a -> do
        liftIO $ writeIORef aRef a
        fire
      pure (c1 *> c2)
  return $ Dynamic r u

instance Functor Event where
  fmap f (Event s) = Event \sel k -> s sel . (. f) $ k
  {-# INLINE fmap #-}

instance Semigroup a => Semigroup (Event a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

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
    c1 <- unEvent eF s \f -> do
      liftIO $ writeIORef latestF (Just f)
      fire
    c2 <- unEvent eA s \a -> do
      liftIO $ writeIORef latestA (Just a)
      fire
    pure (c1 *> c2)

instance Functor Dynamic where
  fmap f d = unsafePerformIO $ fmapHold f d
  {-# NOINLINE fmap #-}

instance Applicative Dynamic where
  pure = constDyn
  (<*>) df da = unsafePerformIO $ splatIO df da
  {-# NOINLINE (<*>) #-}

instance Semigroup x => Semigroup (Reactive x) where
  (<>) = liftA2 (<>)

instance Monoid x => Monoid (Reactive x) where
  mempty = pure mempty

instance Semigroup ReactiveState where
  (<>) (ReactiveState a) (ReactiveState b) = ReactiveState (a <> b)

instance Monoid ReactiveState where
  mempty = ReactiveState mempty
