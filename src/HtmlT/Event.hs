-- | Very simple FRP-like functionality implemented for internal
-- use. 'Event' and 'Dynamic' are similar to the same concepts from
-- Reflex unlike 'DynRef' which is another important definition, not
-- used in other FRP libraries. Some functions also bear the same name
-- as their Reflex counterparts to make API easier to learn
module HtmlT.Event where

import Control.Applicative
import Control.Lens (Lens', over)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Exts
import GHC.Generics
import Unsafe.Coerce
import qualified Data.Map as M

import HtmlT.IdSupply
import qualified HtmlT.HashMap as H

-- | Stream of event occurences of type @a@. Actual representation is
-- just a function that subscribes to the event and returns the action
-- to unsubscribe.
newtype Event a = Event
  { unEvent :: Callback a -> IO Canceller
  } deriving stock Generic

-- | Holds a value that changes over the lifetime of the
-- program. It is possible to read the current value of a 'Dynamic'
-- and subscribe to its future changes.
data Dynamic a = Dynamic
  { dynamic_read :: IO a
  -- ^ Read current value. Use public alias 'readDyn' instead
  , dynamic_updates :: Event a
  -- ^ Event that fires when the value changes. Use public alias
  -- 'updates' instead
  } deriving stock Generic

-- | Mutable variable that supports subscription to its changes. Has
-- similar API to 'IORef' (see 'readRef', 'writeRef', 'modifyRef')
data DynRef a = DynRef
  { dynref_dynamic :: Dynamic a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , dynref_modifier :: Modifier a
  -- ^ Funtion to update the value
  } deriving stock Generic

-- | State inside 'Reactive'
newtype ReactiveState = ReactiveState
  { unReactiveState :: M.Map (Id Event) (Reactive ())
  } deriving stock Generic

-- | Evaluation of effects triggered by event firing
newtype Reactive a = Reactive (StateT ReactiveState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO
    , MonadState ReactiveState, MonadFix, MonadCatch, MonadThrow
    , MonadMask)

type Callback a = a -> Reactive ()

type Trigger a = a -> Reactive ()

type Modifier a = (a -> a) -> Reactive ()

type Canceller = IO ()

class MonadSubscribe m where
  askSubscribe :: m Subscriptions

class MonadFinalize m where
  askFinalizers :: m Finalizers

newtype Subscriptions = Subscriptions
  { unSubscriptions :: H.HashMap (Id Event) [IORef (Callback Any)]
  }

newtype Finalizers = Finalizers {unFinalizers :: IORef [Canceller]}

type MonadReactive m = (MonadIO m, MonadSubscribe m, MonadFinalize m)

newtype SubscribeT m a = SubscribeT {unSubscribeT :: ReaderT Subscriptions m a}
  deriving newtype (MonadIO, Monad, Functor, Applicative)

runSubscribeT :: Subscriptions -> SubscribeT m a -> m a
runSubscribeT s = (`runReaderT` s) . unSubscribeT

-- | Create new event and a function to supply values to that event
--
-- > (event, push) <- newEvent @String
-- > push "New Value" -- event fires with given value
newEvent :: forall a m. (MonadIO m, MonadSubscribe m) => m (Event a, Trigger a)
newEvent = do
  subs <- askSubscribe
  eventId <- liftIO $ nextId @Event
  let event = Event $ subscribeImpl subs eventId
  return (event, triggerImpl subs eventId)

-- | Create new 'DynRef' using given initial value
--
-- > showRef <- newRef False
-- > writeRef showRef True -- update event fires for showRef
newRef :: forall a m. (MonadIO m, MonadSubscribe m) => a -> m (DynRef a)
newRef initial = do
  ref <- liftIO $ newIORef initial
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
readRef = readDyn . dynref_dynamic
{-# INLINE readRef #-}

-- | Same as 'readRef' but also applies given function to the value
-- held by 'DynRef'
readsRef :: MonadIO m => (a -> b) -> DynRef a -> m b
readsRef f = readsDyn f . dynref_dynamic
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
modifySync = dynref_modifier
{-# INLINE modifySync #-}

-- | Version of 'writeRef' that runs inside @Reactive@
writeSync :: DynRef a -> a -> Reactive ()
writeSync ref a = modifySync ref (const a)
{-# INLINE writeSync #-}

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = dynref_dynamic
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

-- | Attach a listener to an event and return an action to detach the
-- listener
subscribe :: MonadReactive m => Event a -> Callback a -> m Canceller
subscribe (Event s) k = do
  subs <- askSubscribe
  fins <- askFinalizers
  cancel <- liftIO $ s k
  liftIO $ modifyIORef (unFinalizers fins) (cancel:)
  return cancel

-- | Perform an action with current value of the given 'Dynamic' and
-- each time the value changes. Return action to detach listener from
-- receiving new values
forDyn :: MonadReactive m => Dynamic a -> Callback a -> m Canceller
forDyn d k = do
  liftIO $ dynamic_read d >>= sync . k
  subscribe (dynamic_updates d) k

-- | Same as 'forDyn', ignore the result
forDyn_ :: MonadReactive m => Dynamic a -> Callback a -> m ()
forDyn_ = (void .) . forDyn

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f e = Event \k -> unEvent e $ maybe mempty k . f
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
holdUniqDynBy equal Dynamic{..} = dynamic where
  updates = mapMaybeE id $ Event \k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates \new -> do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef new
      k if old `equal` new then Nothing else Just new
  dynamic = Dynamic dynamic_read updates

-- | Print a debug message when given event fires
traceEvent :: Show a => String -> Event a -> Event a
traceEvent = traceEventWith show
{-# INLINE traceEvent #-}

-- | Print a debug message when the event fires using given printing
-- function
traceEventWith :: (a -> String) -> String -> Event a -> Event a
traceEventWith show' tag (Event f) = Event \c ->
  f (c . (\x -> trace (tag ++ ": " ++ show' x) x))
{-# INLINE traceEventWith #-}

-- | Print a debug message when value inside Dynamic changes
traceDyn :: Show a => String -> Dynamic a -> Dynamic a
traceDyn = traceDynWith show
{-# INLINE traceDyn #-}

-- | Print a debug message when value inside Dynamic changes
traceRef :: Show a => String -> DynRef a -> DynRef a
traceRef s DynRef{..} = DynRef
  {dynref_dynamic = traceDynWith show s dynref_dynamic, ..}

-- | Print a debug message when value inside Dynamic changes using
-- given printing function
traceDynWith :: (a -> String) -> String -> Dynamic a -> Dynamic a
traceDynWith show' tag d = d {dynamic_updates = e} where
  e = traceEventWith show' tag (dynamic_updates d)
{-# INLINE traceDynWith #-}

-- | Make an 'Event' with a tuple containing previous value as well as
-- the current value from the original event
diffEvent :: a -> Event a -> Event (a, a)
diffEvent initial (Event s) = Event \k -> do
  oldRef <- liftIO (newIORef initial)
  let
    f a = do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef a
      k (old, a)
  s f

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event
mapDyn :: MonadReactive m => Dynamic a -> (a -> b) -> m (Dynamic b)
mapDyn dynA f = do
  initialA <- liftIO $ dynamic_read dynA
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef (f initialA)
  eventId <- liftIO $ nextId @Event
  subs <- askSubscribe
  let
    updates = Event $ subscribeImpl subs eventId
    fire = defer eventId do
      newB <- liftIO $ f <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      triggerImpl subs eventId newB
  dynamic_updates dynA `subscribe` \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  return $ Dynamic (readIORef latestB) updates

-- | Same as 'mapDyn' but with two Dynamics, @f@ is called each time
-- any of the two Dynamics changes its value
mapDyn2
  :: MonadReactive m => Dynamic a -> Dynamic b -> (a -> b -> c) -> m (Dynamic c)
mapDyn2 aDyn bDyn f = do
  initialA <- liftIO $ dynamic_read aDyn
  initialB <- liftIO $ dynamic_read bDyn
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef initialB
  latestC <- liftIO $ newIORef (f initialA initialB)
  eventId <- liftIO $ nextId @Event
  subs <- askSubscribe
  let
    fire = defer eventId do
      newC <- liftIO $ liftA2 f (readIORef latestA) (readIORef latestB)
      liftIO $ writeIORef latestC newC
      triggerImpl subs eventId newC
    updates = Event $ subscribeImpl subs eventId
  dynamic_updates aDyn `subscribe` \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  dynamic_updates bDyn `subscribe` \newB -> do
    liftIO $ writeIORef latestB newB
    defer eventId fire
  return $ Dynamic (readIORef latestC) updates

subscribeImpl :: Subscriptions -> Id Event -> Callback a -> IO Canceller
subscribeImpl (Subscriptions ht) eventId k = do
  kref <- newIORef (\(a::Any) -> k (unsafeCoerce a)) -- Need 'IORef' for an 'Eq' instance
  H.mutate ht eventId \mss ->
    (Just (kref : fromMaybe [] mss), ())
  pure $ H.mutate ht eventId \mss ->
    (mfilter (/=[]) $ Just (delete kref $ fromMaybe [] mss), ())

triggerImpl :: Subscriptions -> Id Event -> a -> Reactive ()
triggerImpl (Subscriptions ht) eventId a = defer eventId do
  callbacks <- liftIO $ fromMaybe [] <$> H.lookup ht eventId
  for_ callbacks $ liftIO . readIORef >=> ($ unsafeCoerce @_ @Any a)

instance Applicative m => MonadSubscribe (SubscribeT m) where
  askSubscribe = SubscribeT $ ReaderT pure

instance Functor Event where
  fmap f (Event s) = Event \k -> s . (. f) $ k
  {-# INLINE fmap #-}

instance Semigroup a => Semigroup (Event a) where
  (<>) (Event e1) (Event e2) = Event \k -> do
    eventId <- nextId @Event
    c1 <- e1 (defer eventId . k)
    c2 <- e2 (defer eventId . k)
    return (c1 *> c2)

instance Semigroup a => Monoid (Event a) where
  mempty = never

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) df da = Dynamic read updates where
    read = liftA2 ($) (dynamic_read df) (dynamic_read da)
    updates = Event \k -> do
      eventId <- nextId @Event
      let
        fire newF newA = defer eventId do
          f <- liftIO $ maybe (dynamic_read df) pure newF
          a <- liftIO $ maybe (dynamic_read da) pure newA
          k (f a)
      c1 <- dynamic_updates df `unEvent` \f ->
        fire (Just f) Nothing
      c2 <- dynamic_updates da `unEvent` \a ->
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
