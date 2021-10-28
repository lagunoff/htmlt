-- | Simple FRP-like functionality. This module implements 'Event's
-- and 'Dynamic's similar to the same concepts from Reflex. Also there
-- is 'DynRef' a new definition which is expected to be used alongside
-- Dynamics and Events in a typical application. Some functions bear
-- the same name as their Reflex counterparts to make API easier to
-- learn
module HtmlT.Event where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.Maybe
import Debug.Trace
import GHC.Exts
import GHC.Generics
import Unsafe.Coerce
import qualified Data.Map as M

-- | Stream of event occurences of type @a@. Actual representation is
-- just a function that subscribes to the event and returns the action
-- to cancel the subscription.
newtype Event a = Event
  { unEvent :: ReactiveEnv -> Callback a -> IO Canceller
  } deriving stock Generic

-- | Holds a value that changes over the time. Allows to read the
-- current value and subscribe to its future changes.
data Dynamic a = Dynamic
  { dynamic_read :: IO a
  -- ^ Read current value. Use public alias 'readDyn' instead
  , dynamic_updates :: Event a
  -- ^ Event that fires when the value changes. Use public alias
  -- 'updates' instead
  } deriving stock Generic

-- | Mutable variable that supports subscribing to new values. Has
-- similar API to 'IORef' (see 'readRef', 'writeRef', 'modifyRef')
data DynRef a = DynRef
  { dynref_dynamic :: Dynamic a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , dynref_modifier :: Modifier a
  -- ^ Funtion to update the value
  } deriving stock Generic

-- | State inside 'Transact'
newtype TransactState = TransactState
  { unTransactState :: M.Map EventId (Transact ())
  } deriving Generic

-- | Evaluation of effects triggered by an event firing
newtype Transact a = Transact (StateT TransactState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO
    , MonadState TransactState, MonadFix, MonadCatch, MonadThrow
    , MonadMask)

-- | The environment required for some operations like creating a new
-- 'Event' or subscribing to an Event.
data ReactiveEnv = ReactiveEnv
  { renv_subscriptions :: IORef (M.Map EventId [(SubscriptionId, Callback Any)])
  -- ^ Keeps track of subscriptions
  , renv_finalizers :: IORef [Canceller]
  -- ^ List of cancellers, IO actions to detach listeners from
  -- events. It is likely that dynamic parts of the application will
  -- be passed new instance of 'renv_finalizers' to isolate and remove
  -- the subscriptions after this part is detached
  , renv_id_generator :: IORef Int
  -- ^ Contains next value for 'EventId' or 'SubscriptionId'
  } deriving Generic

-- | Minimal implementation for 'HasReactiveEnv'
newtype ReactiveT m a = ReactiveT
  { unReactiveT :: ReaderT ReactiveEnv m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO
    , MonadFix, MonadCatch, MonadThrow, MonadMask)

-- | Identify events inside 'TransactState' and 'ReactiveEnv'
newtype EventId = EventId {unEventId :: Int}
  deriving newtype (Eq, Ord, Show)

-- | Identify subscriptions inside 'renv_subscriptions'
newtype SubscriptionId = SubscriptionId {unSubscriptionId :: Int}
  deriving newtype (Eq, Ord, Show)

class HasReactiveEnv m where askReactiveEnv :: m ReactiveEnv

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Callback a = a -> Transact ()

type Trigger a = a -> Transact ()

type Modifier a = (a -> a) -> Transact ()

type Canceller = IO ()

type MonadReactive m = (HasReactiveEnv m, MonadIO m)

-- | Create new empty 'ReactiveEnv'
newReactiveEnv :: MonadIO m => m ReactiveEnv
newReactiveEnv = liftIO do
  renv_finalizers <- newIORef []
  renv_subscriptions <- newIORef M.empty
  renv_id_generator <- newIORef 0
  return ReactiveEnv{..}

-- | Create new event and a function to supply values to that event
--
-- > (event, push) <- newEvent @String
-- > push "New Value" -- event fires with given value
newEvent :: forall a m. MonadReactive m => m (Event a, Trigger a)
newEvent = do
  renv <- askReactiveEnv
  eventId <- EventId <$> liftIO (nextIntId renv)
  let event = Event $ subscribeImpl eventId
  return (event, triggerImpl eventId renv)

-- | Create new 'DynRef' using given initial value
--
-- > showRef <- newRef False
-- > writeRef showRef True -- update event fires for showRef
newRef :: forall a m. MonadReactive m => a -> m (DynRef a)
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

-- | Version of 'writeRef' that runs inside @Transact@
writeSync :: DynRef a -> a -> Transact ()
writeSync ref a = modifySync ref (const a)

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . dynref_dynamic

-- | Same as 'readRef' but also applies given function to the value
-- held by 'DynRef'
readsRef :: MonadIO m => (a -> b) -> DynRef a -> m b
readsRef f = readsDyn f . dynref_dynamic

-- | Update a 'DynRef' by applying a given function to the current
-- value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: MonadIO m => DynRef a -> (a -> a) -> m ()
modifyRef (DynRef _ modifier) = liftIO . sync . modifier

-- | Version of 'modifyRef' that runs inside @Transact@
modifySync :: DynRef a -> (a -> a) -> Transact ()
modifySync = dynref_modifier

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = dynref_dynamic

-- | Read the value held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . dynamic_read

-- | Same as 'readDyn' but also applies given function to the value
-- held by 'Dynamic'
readsDyn :: MonadIO m => (a -> b) -> Dynamic a -> m b
readsDyn f = fmap f . liftIO . dynamic_read

-- | Extract the updates Event from a 'Dynamic'
updates :: Dynamic a -> Event a
updates = dynamic_updates

-- | Attach a listener to the event and return an action to detach the
-- listener
subscribe :: MonadReactive m => Event a -> Callback a -> m Canceller
subscribe (Event s) k = do
  e@ReactiveEnv{..} <- askReactiveEnv
  cancel <- liftIO $ s e k
  liftIO $ modifyIORef' renv_finalizers (cancel:)
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
mapMaybeE f e = Event \r k -> unEvent e r (maybe (pure ()) k . f)
{-# INLINE mapMaybeE #-}

-- | Apply a lens to the value inside 'DynRef'
lensMap :: Lens' s a -> DynRef s -> DynRef a
lensMap l (DynRef d m) = DynRef (Dynamic read updates) modify where
  read = fmap (getConst . l Const) $ dynamic_read d
  updates = fmap (getConst . l Const) $ dynamic_updates d
  modify f = m (runIdentity . l (Identity . f))

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equal Dynamic{..} = Dynamic dynamic_read
  (mapMaybeE id $ Event \e k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates e \new -> do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef new
      k if old `equal` new then Nothing else Just new
  )

-- | Contruct a 'DynRef' containing a tuple from two distinct
-- 'DynRef's
zipRef :: DynRef a -> DynRef b -> DynRef (a, b)
zipRef aRef bRef = DynRef
  (liftA2 (,) (fromRef aRef) (fromRef bRef))
  (\f -> do
    oldA <- readRef aRef
    oldB <- readRef bRef
    let (newA, newB) = f (oldA, oldB)
    writeSync aRef newA
    writeSync bRef newB
  )

-- | Contruct a 'DynRef' containing a 3-tuple from three distinct
-- 'DynRef's
zipRef3 :: DynRef a -> DynRef b -> DynRef c -> DynRef (a, b, c)
zipRef3 aRef bRef cRef = DynRef
  (liftA3 (,,) (fromRef aRef) (fromRef bRef) (fromRef cRef))
  (\f -> do
    oldA <- readRef aRef
    oldB <- readRef bRef
    oldC <- readRef cRef
    let (newA, newB, newC) = f (oldA, oldB, oldC)
    writeSync aRef newA
    writeSync bRef newB
    writeSync cRef newC
  )

-- | Print a debug message each time given event fires
traceEvent :: Show a => String -> Event a -> Event a
traceEvent = traceEventWith show

-- | Print a debug message when the event fires using given printing
-- function
traceEventWith :: (a -> String) -> String -> Event a -> Event a
traceEventWith show' tag (Event f) = Event \e c ->
  f e (c . (\x -> trace (tag ++ ": " ++ show' x) x))

-- | Print a debug message when value inside the Dynamic changes
traceDyn :: Show a => String -> Dynamic a -> Dynamic a
traceDyn = traceDynWith show

-- | Print a debug message when value inside the DynRef changes
traceRef :: Show a => String -> DynRef a -> DynRef a
traceRef s DynRef{..} = DynRef
  {dynref_dynamic = traceDynWith show s dynref_dynamic, ..}

-- | Print a debug message when value inside Dynamic changes using
-- given printing function
traceDynWith :: (a -> String) -> String -> Dynamic a -> Dynamic a
traceDynWith show' tag d = d {dynamic_updates = e} where
  e = traceEventWith show' tag (dynamic_updates d)

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event
mapDyn
  :: MonadReactive m
  => Dynamic a
  -> (a -> b)
  -> m (Dynamic b)
mapDyn dynA f = do
  initialA <- liftIO $ dynamic_read dynA
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef (f initialA)
  renv <- askReactiveEnv
  eventId <- EventId <$> liftIO (nextIntId renv)
  let
    updates = Event $ subscribeImpl eventId
    fire = defer eventId do
      newB <- liftIO $ f <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      triggerImpl eventId renv newB
  dynamic_updates dynA `subscribe` \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  return $ Dynamic (readIORef latestB) updates

-- | Same as 'mapDyn' but with two Dynamics, @f@ is called each time
-- any of the two Dynamics changes its value
-- TODO: More general version of mapDynX
mapDyn2
  :: MonadReactive m
  => Dynamic a
  -> Dynamic b
  -> (a -> b -> c)
  -> m (Dynamic c)
mapDyn2 aDyn bDyn f = do
  initialA <- liftIO $ dynamic_read aDyn
  initialB <- liftIO $ dynamic_read bDyn
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef initialB
  latestC <- liftIO $ newIORef (f initialA initialB)
  renv <- askReactiveEnv
  eventId <- EventId <$> liftIO (nextIntId renv)
  let
    fire = defer eventId do
      newC <- liftIO $ liftA2 f (readIORef latestA) (readIORef latestB)
      liftIO $ writeIORef latestC newC
      triggerImpl eventId renv newC
    updates = Event $ subscribeImpl eventId
  dynamic_updates aDyn `subscribe` \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  dynamic_updates bDyn `subscribe` \newB -> do
    liftIO $ writeIORef latestB newB
    defer eventId fire
  return $ Dynamic (readIORef latestC) updates

runReactiveT :: ReactiveEnv -> ReactiveT m a -> m a
runReactiveT s = (`runReaderT` s) . unReactiveT

-- | Read and increment 'renv_id_generator'
nextIntId :: ReactiveEnv -> IO Int
nextIntId ReactiveEnv{..} = atomicModifyIORef'
  renv_id_generator \x -> (succ x, x)

-- | Defer a computation (usually an event firing) till the end of
-- current reactive transaction. This makes possible to avoid double
-- firing of events, constructed from multiple other events
defer :: EventId -> Transact () -> Transact ()
defer k act = Transact (modify f) where
  f (TransactState s) = TransactState (M.insert k act s)

-- | Run a reactive transaction.
sync :: MonadIO m => Transact a -> m a
sync act = liftIO $ loop (TransactState M.empty) act where
  loop :: TransactState -> Transact a -> IO a
  loop rs (Transact act) = do
    (r, newRs) <- runStateT act rs
    case popQueue newRs of
      (Just newAct, newerRs) -> r <$ loop newerRs newAct
      (Nothing, newerRs)     -> return r
  popQueue intact@(TransactState m) = case M.minViewWithKey m of
    Just ((_, act), rest) -> (Just act, TransactState rest)
    Nothing               -> (Nothing, intact)

subscribeImpl :: EventId -> ReactiveEnv -> Callback a -> IO Canceller
subscribeImpl eventId e@ReactiveEnv{..} k = do
  subsId <- SubscriptionId <$> nextIntId e
  let newCancel = (subsId, k . unsafeCoerce)
  alterSubs $ Just . (newCancel :) . fromMaybe []
  return $ alterSubs $
    mfilter (not . Prelude.null) . Just . deleteSub subsId . fromMaybe []
  where
    alterSubs = modifyIORef' renv_subscriptions . flip M.alter eventId
    deleteSub sId [] = []
    deleteSub sId ((xId, c):xs)
      | sId == xId = xs
      | otherwise = (xId, c) : deleteSub sId xs

triggerImpl :: EventId -> ReactiveEnv -> a -> Transact ()
triggerImpl eventId ReactiveEnv{..} a = defer eventId do
  subscriptions <- liftIO $ readIORef renv_subscriptions
  let callbacks = fromMaybe [] $ M.lookup eventId subscriptions
  for_ callbacks $ ($ unsafeCoerce @_ @Any a) . snd

instance Functor Event where
  fmap f (Event s) = Event \e k -> s e . (. f) $ k

instance Semigroup a => Semigroup (Event a) where
  (<>) (Event e1) (Event e2) = Event \e k -> do
    eventId <- EventId <$> nextIntId e
    c1 <- e1 e (defer eventId . k)
    c2 <- e2 e (defer eventId . k)
    return (c1 *> c2)

instance Semigroup a => Monoid (Event a) where
  mempty = never

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) df da = Dynamic
    (liftA2 ($) (readDyn df) (readDyn da))
    (Event \e k -> do
      eventId <- EventId <$> nextIntId e
      let
        fire newF newA = defer eventId do
          f <- liftIO $ maybe (readDyn df) pure newF
          a <- liftIO $ maybe (readDyn da) pure newA
          k (f a)
      c1 <- unEvent (updates df) e \f -> fire (Just f) Nothing
      c2 <- unEvent (updates da) e \a -> fire Nothing (Just a)
      return (c1 *> c2)
    )

instance Semigroup TransactState where
  (<>) (TransactState a) (TransactState b) = TransactState (a <> b)

instance Monoid TransactState where
  mempty = TransactState mempty

instance Applicative m => HasReactiveEnv (ReactiveT m) where
  askReactiveEnv = ReactiveT $ ReaderT pure
