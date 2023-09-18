{-|

This module offers clear and straightforward implementation of FRP
concepts such as Events and Dynamics, inspired by
Reflex. Additionally, it introduces DynRefs, which are represented by
a Dynamic along with a function to modify the value inside the
Dynamic. DynRef has similar inferface to IORef with functions like
readRef, writeRef, modifyRef etc.
-}
module HtmlT.Event where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple
import GHC.Exts
import GHC.Fingerprint
import GHC.Generics
import Unsafe.Coerce

-- | Represents a stream of event occurrences of type @a@. Its actual
-- representation is simply a function that subscribes to the event
newtype Event a = Event
  { unEvent :: ReactiveEnv -> Callback a -> IO ()
  }

-- | Contains a value that is subject to change over time. Provides
-- operations for reading the current value ('readDyn') and
-- subscribing to its future changes ('updates').
data Dynamic a = Dynamic
  { dynamic_read :: IO a
  -- ^ Read current value. Use public alias 'readDyn' instead
  , dynamic_updates :: Event a
  -- ^ Event that fires when the value changes. Use public alias
  -- 'updates' instead
  } deriving stock Generic

-- | A mutable variable that allows for subscription to new values. It
-- shares a similar API to 'IORef' (see 'readRef', 'writeRef',
-- 'modifyRef')
data DynRef a = DynRef
  { dynref_dynamic :: Dynamic a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , dynref_modifier :: Modifier a
  -- ^ Funtion to update the value
  } deriving stock Generic

-- | Function that updates the value inside the 'DynRef'
newtype Modifier a = Modifier
  { unModifier :: forall r. Bool -> (a -> (a, r)) -> Step r
  -- ^ 'Bool' argument controls whether the modification should
  -- trigger an update event. It's possible to update the 'DynRef'
  -- without notifying the subscribers for optimization purposes, in
  -- cases when you know that all changes already been reflected in
  -- the DOM
  }

-- | State inside 'Step'
newtype TransactState = TransactState
  { unTransactState :: Map QueueId (Step ())
  } deriving newtype (Semigroup, Monoid)

-- | Evaluation of effects triggered by an event firing
newtype Step a = Step { unStep :: StateT TransactState IO a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadState TransactState, MonadFix
    , MonadCatch, MonadThrow, MonadMask
    )

-- | Represents the environment necessary for "reactive" operations,
-- such as creating a new 'Event', subscribing to an event etc
data ReactiveEnv = ReactiveEnv
  { renv_subscriptions :: IORef (Map QueueId [(QueueId, Callback Any)])
  -- ^ Keeps track of subscriptions
  , renv_finalizers :: IORef (Map FinalizerKey FinalizerValue)
  -- ^ Keeps track of finalizers. These finalizers will be activated
  -- shortly before the current part of the application is terminated.
  , renv_id_generator :: IORef QueueId
  -- ^ Maintains the next value to be used for generating 'QueueId'
  } deriving Generic

-- | Minimal implementation for 'HasReactiveEnv'
newtype ReactiveT m a = ReactiveT
  { unReactiveT :: ReaderT ReactiveEnv m a
  } deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadFix, MonadCatch, MonadThrow
    , MonadMask
    )

-- | Identifies a computation inside 'TransactState'. The integer
-- value within 'QueueId' dictates the execution order in a reactive
-- transaction (with higher values executing later). It is also
-- utilized to prioritize events derived from other events, ensuring
-- they are processed after the source events. This is basically the
-- mechanism that prevents double-firing of Dynamics constructed
-- using, for instance, the Applicative instance.
newtype QueueId = QueueId {unQueueId :: Int}
  deriving newtype (Eq, Show, Ord, Num, Enum, Bounded)

data FinalizerKey
  = FinalizerEventId QueueId
  | FinalizerQueueId QueueId
  | FinalizerFingerprintId Fingerprint
  deriving (Eq, Ord, Generic)

data FinalizerValue
  = SubscriptionSet (Set QueueId)
  | CustomFinalizer (IO ())
  deriving Generic

class HasReactiveEnv m where askReactiveEnv :: m ReactiveEnv

type MonadReactive m = (HasReactiveEnv m, MonadIO m)

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Callback a = a -> Step ()

type Trigger a = a -> Step ()

-- | Create new empty 'ReactiveEnv'
newReactiveEnv :: MonadIO m => m ReactiveEnv
newReactiveEnv = liftIO do
  renv_finalizers <- newIORef Map.empty
  renv_subscriptions <- newIORef Map.empty
  renv_id_generator <- newIORef $ QueueId 0
  return ReactiveEnv {..}

-- | Create new event and a function to supply values to that event
--
-- > (event, push) <- newEvent @String
-- > push "New Value" -- event fires with given value
newEvent :: forall a m. MonadReactive m => m (Event a, Trigger a)
newEvent = do
  renv <- askReactiveEnv
  eventId <- liftIO (nextQueueId renv)
  let event = Event $ unsafeSubscribe eventId
  return (event, unsafeTrigger eventId renv)

-- | Create a new 'DynRef' using given initial value
--
-- > showRef <- newRef False
-- > dynStep $ writeRef showRef True -- this triggers update event for showRef
newRef :: forall a m. MonadReactive m => a -> m (DynRef a)
newRef initial = do
  ref <- liftIO $ newIORef initial
  (event, push) <- newEvent
  let
    modify = Modifier \u f -> do
      (new, result) <- liftIO $ atomicModifyIORef' ref \old ->
        let (new, result) = f old in
          (new, (new, result))
      when u $ push new
      return result
    dynamic = Dynamic (readIORef ref) event
  return $ DynRef dynamic modify

-- | Create a Dynamic that never changes its value
constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

-- | Event that will never fire
never :: Event a
never = Event \_ _ -> return ()

-- | Write new value into a 'DynRef'
--
-- > ref <- newRef "Initial value"
-- > transactionWrite ref "New value"
-- > readRef ref
-- "New value"
writeRef :: DynRef a -> a -> Step ()
writeRef ref a = modifyRef ref (const a)

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . dynref_dynamic

-- | Update a 'DynRef' by applying given function to the current value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: DynRef a -> (a -> a) -> Step ()
modifyRef (DynRef _ (Modifier mod)) f = mod True $ (,()) . f

-- | Update a 'DynRef' with first field of the tuple and return back
-- the second field. The name is intended to be similar to
-- 'atomicModifyIORef' but there are no atomicity guarantees
-- whatsoever
atomicModifyRef :: DynRef a -> (a -> (a, r)) -> Step r
atomicModifyRef (DynRef _ (Modifier mod)) f = mod True f

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = dynref_dynamic

-- | Read the value held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . dynamic_read

-- | Extract the updates Event from a 'Dynamic'
updates :: Dynamic a -> Event a
updates = dynamic_updates

-- | Attach a listener to the event and return an action to detach the
-- listener
subscribe :: MonadReactive m => Event a -> Callback a -> m ()
subscribe (Event s) k = do
  re <- askReactiveEnv
  liftIO $ s re k

-- | Executes an action currently held inside the 'Dynamic' and every
-- time the value changes.
performDyn :: MonadReactive m => Dynamic (Step ()) -> m ()
performDyn d = do
  liftIO $ dynamic_read d >>= dynStep
  subscribe (dynamic_updates d) id

-- | Apply a lens to the value inside 'DynRef'
lensMap :: forall s a. Lens' s a -> DynRef s -> DynRef a
lensMap l (DynRef sdyn (Modifier smod)) =
  DynRef adyn (Modifier amod)
    where
      adyn = Dynamic
        (fmap (getConst . l Const) $ dynamic_read sdyn)
        (fmap (getConst . l Const) $ dynamic_updates sdyn)
      amod :: forall r. Bool -> (a -> (a, r)) -> Step r
      amod u f = smod u $ swap . l (swap . f)

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equalFn Dynamic{..} = Dynamic dynamic_read
  (Event \e k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates e \new -> do
      old <- liftIO $ atomicModifyIORef' oldRef (new,)
      unless (old `equalFn` new) $ k new
  )

-- | Execute the gives finalizers
applyFinalizer :: ReactiveEnv -> Map FinalizerKey FinalizerValue -> IO ()
applyFinalizer ReactiveEnv{renv_subscriptions} finalizers = do
  forM_ (Map.toList finalizers) \(k, v) -> case (k, v) of
    (FinalizerEventId e, SubscriptionSet s) ->
      modifyIORef' renv_subscriptions $
        flip Map.alter e $ mfilter (not . Prelude.null) . Just . deleteSubs s . fromMaybe []
    (_, CustomFinalizer io) ->
      io
    (_, _) ->
      return ()
  where
    deleteSubs _ss [] = []
    deleteSubs ss ((s, c):xs)
      | Set.member s ss = xs
      | otherwise = (s, c) : deleteSubs ss xs

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event. As
-- a general guideline, if the function @f! is inexpensive, opt for
-- using @fmap f@. Otherwise, consider using @mapDyn f@.
mapDyn
  :: MonadReactive m
  => (a -> b)
  -> Dynamic a
  -> m (Dynamic b)
mapDyn fun adyn = do
  initialA <- liftIO $ dynamic_read adyn
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef (fun initialA)
  renv <- askReactiveEnv
  eventId <- liftIO (nextQueueId renv)
  let
    updates = Event $ unsafeSubscribe eventId
    fire = defer eventId do
      newB <- liftIO $ fun <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      unsafeTrigger eventId renv newB
  dynamic_updates adyn `subscribe` \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  return $ Dynamic (readIORef latestB) updates

-- | Works same way as 'mapDyn' but applies to two dynamics
mapDyn2
  :: MonadReactive m
  => (a -> b -> c)
  -> Dynamic a
  -> Dynamic b
  -> m (Dynamic c)
mapDyn2 f adyn bdyn = do
  unsafeMapDynN g [unsafeCoerce adyn, unsafeCoerce bdyn]
  where
    g [a, b] = return $ f (unsafeCoerce a) (unsafeCoerce b)
    g _ = error "mapDyn2: impossible happend!"

-- | I hope three arguments will be enough for most cases if more
-- needed it's easy to define this function in the application code
-- with any required arity
mapDyn3
  :: MonadReactive m
  => (a -> b -> c -> d)
  -> Dynamic a
  -> Dynamic b
  -> Dynamic c
  -> m (Dynamic d)
mapDyn3 f adyn bdyn cdyn = do
  unsafeMapDynN g
    [unsafeCoerce adyn, unsafeCoerce bdyn, unsafeCoerce cdyn]
  where
    g [a, b, c] = return $ f (unsafeCoerce a) (unsafeCoerce b) (unsafeCoerce c)
    g _ = error "mapDyn3: impossible happend!"

-- | Takes a list of Dynamics and a function to generate the
-- output. The positions of elements in the list of [Any] received by
-- the function always correspond to the positions of [Dynamic Any]
-- from which these values were generated. The Dynamic created by this
-- function will fire at most once per transaction, and only if any of
-- the input Dynamics change their values.
unsafeMapDynN
  :: MonadReactive m
  => ([Any] -> IO a)
  -- ^ Construct the output value, from list of input values from
  -- corresponding positions of given Dynamics
  -> [Dynamic Any]
  -- ^ List of input Dynamics
  -> m (Dynamic a)
unsafeMapDynN fun dyns = do
  renv <- askReactiveEnv
  -- TODO: Try if list of IORefs is better than IORef of list
  initialInputs <- liftIO $ mapM dynamic_read dyns
  initialOutput <- liftIO $ fun initialInputs
  latestInputsRef <- liftIO $ newIORef initialInputs
  latestOutputRef <- liftIO $ newIORef initialOutput
  eventId <- liftIO (nextQueueId renv)
  let
    fire = defer eventId do
      newOutput <- liftIO $ fun =<< readIORef latestInputsRef
      liftIO $ writeIORef latestOutputRef newOutput
      unsafeTrigger eventId renv newOutput
    updates = Event $ unsafeSubscribe eventId
    updateList _ _ [] = []
    updateList 0 a (_:xs) = a:xs
    updateList n a (x:xs) = x : updateList (pred n) a xs
  forM_ (zip [0..] dyns) \(i::Int, adyn) -> do
    dynamic_updates adyn `subscribe` \newVal -> do
      liftIO $ modifyIORef latestInputsRef $ updateList i newVal
      defer eventId fire
  return $ Dynamic (readIORef latestOutputRef) updates

-- | Read and increment 'renv_id_generator'
nextQueueId :: ReactiveEnv -> IO QueueId
nextQueueId ReactiveEnv{renv_id_generator} =
  atomicModifyIORef' renv_id_generator \eid -> (succ eid, eid)

-- | Defers a computation (typically an event firing) until the end of
-- the current reactive transaction. This allows for the avoidance of
-- double firing of events constructed from multiple other events.
defer :: QueueId -> Step () -> Step ()
defer k act =
  Step $ modify \(TransactState s) -> TransactState (Map.insert k act s)

-- | Run a reactive transaction.
dynStep :: MonadIO m => Step a -> m a
dynStep act = liftIO $ loop (TransactState Map.empty) act where
  loop :: TransactState -> Step a -> IO a
  loop rs (Step act) = do
    (r, newRs) <- runStateT act rs
    case popQueue newRs of
      (Just newAct, newerRs) -> r <$ loop newerRs newAct
      (Nothing, _newerRs) -> return r
  popQueue intact@(TransactState m) = case Map.minViewWithKey m of
    Just ((_, act), rest) -> (Just act, TransactState rest)
    Nothing -> (Nothing, intact)

runReactiveT :: ReactiveT m a -> ReactiveEnv -> m a
runReactiveT r = runReaderT (unReactiveT r)

execReactiveT :: ReactiveEnv -> ReactiveT m a -> m a
execReactiveT = flip runReactiveT

unsafeSubscribe :: QueueId -> ReactiveEnv -> Callback a -> IO ()
unsafeSubscribe eventId e@ReactiveEnv{renv_subscriptions, renv_finalizers} k = do
  subsId <- nextQueueId e
  let
    newCancel = (subsId, k . unsafeCoerce)
    f (SubscriptionSet s1) (SubscriptionSet s2) = SubscriptionSet (s1 <> s2)
    -- Unreacheable because FinalizerEventId always should map into
    -- SubscriptionSet
    f _ s = s
  modifyIORef' renv_subscriptions $
    flip Map.alter eventId $ Just . (newCancel :) . fromMaybe []
  modifyIORef' renv_finalizers $ Map.insertWith f (FinalizerEventId eventId)
    (SubscriptionSet (Set.singleton subsId))

unsafeTrigger :: QueueId -> ReactiveEnv -> a -> Step ()
unsafeTrigger eventId ReactiveEnv{..} a = defer eventId do
  subscriptions <- liftIO $ readIORef renv_subscriptions
  let callbacks = fromMaybe [] $ Map.lookup eventId subscriptions
  for_ callbacks $ ($ unsafeCoerce @_ @Any a) . snd

instance Functor Event where
  fmap f (Event s) = Event \e k -> s e . (. f) $ k

-- | Please be aware that in cases where both events fire during the
-- same 'Step,' the one having a higher 'EventId' will win, which is
-- very hard to predict, use with caution.
instance Semigroup a => Semigroup (Event a) where
  (<>) (Event e1) (Event e2) = Event \e k -> mdo
    e1 e (defer eventId . k)
    e2 e (defer eventId . k)
    eventId <- nextQueueId e
    return ()

instance Semigroup a => Monoid (Event a) where
  mempty = never

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) df da =
    let
      updatesEvent = Event \e k -> mdo
        let
          fire newF newA = defer eventId do
            f <- liftIO $ maybe (readDyn df) pure newF
            a <- liftIO $ maybe (readDyn da) pure newA
            k (f a)
        unEvent (updates df) e \f -> fire (Just f) Nothing
        unEvent (updates da) e \a -> fire Nothing (Just a)
        eventId <- nextQueueId e
        return ()
    in
      Dynamic
        { dynamic_read = liftA2 ($) (dynamic_read df) (dynamic_read da)
        , dynamic_updates = updatesEvent
        }

instance Applicative m => HasReactiveEnv (ReactiveT m) where
  askReactiveEnv = ReactiveT $ ReaderT pure
