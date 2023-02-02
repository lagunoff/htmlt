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
import Data.Tuple
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

-- | State inside 'Step'
newtype TransactState = TransactState
  { unTransactState :: M.Map EventId (Step ())
  } deriving newtype (Semigroup, Monoid)

-- | Evaluation of effects triggered by an event firing
newtype Step a = Step { unStep :: StateT TransactState IO a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadState TransactState, MonadFix
    , MonadCatch, MonadThrow, MonadMask
    )

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
  } deriving newtype
    ( Functor, Applicative, Monad, MonadIO, MonadFix, MonadCatch, MonadThrow
    , MonadMask
    )

-- | Identify events inside 'TransactState' and 'ReactiveEnv'
newtype EventId = EventId {unEventId :: Int}
  deriving newtype (Eq, Ord, Show)

-- | Identify subscriptions inside 'renv_subscriptions'
newtype SubscriptionId = SubscriptionId {unSubscriptionId :: Int}
  deriving newtype (Eq, Ord, Show)

newtype Modifier a = Modifier
  { unModifier :: forall r. Bool -> (a -> (a, r)) -> Step r
  }

class HasReactiveEnv m where askReactiveEnv :: m ReactiveEnv

type MonadReactive m = (HasReactiveEnv m, MonadIO m)

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Callback a = a -> Step ()

type Trigger a = a -> Step ()

type Canceller = IO ()

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
-- > transactionWrite showRef True -- this triggers update event for showRef
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
never = Event \_ -> mempty

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
-- the second field
atomicModifyRef :: DynRef a -> (a -> (a, r)) -> Step r
atomicModifyRef (DynRef _ (Modifier mod)) f = mod True f

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
  env@ReactiveEnv{renv_finalizers} <- askReactiveEnv
  cancel <- liftIO $ s env k
  liftIO $ modifyIORef' renv_finalizers (cancel:)
  return cancel

-- | Perform an action with current value of the given 'Dynamic' and
-- each time the value changes. Return action to detach listener from
-- receiving new values
performDyn :: MonadReactive m => Dynamic a -> Callback a -> m Canceller
performDyn d k = do
  liftIO $ dynamic_read d >>= newStep . k
  subscribe (dynamic_updates d) k

-- | Same as 'performDyn', but ignores the canceller
performDyn_ :: MonadReactive m => Dynamic a -> Callback a -> m ()
performDyn_ = (void .) . performDyn

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f e = Event \r k -> unEvent e r (maybe (pure ()) k . f)
{-# INLINE mapMaybeE #-}

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
holdUniqDynBy equal Dynamic{..} = Dynamic dynamic_read
  (mapMaybeE id $ Event \e k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates e \new -> do
      old <- liftIO (readIORef oldRef)
      liftIO $ writeIORef oldRef new
      k if old `equal` new then Nothing else Just new
  )

-- | Print a debug message each time given event fires
traceEvent :: Show a => String -> Event a -> Event a
traceEvent tag = traceEventWith (((tag <> ": ") <>) . show)

-- | Print a debug message when the event fires using given printing
-- function
traceEventWith :: (a -> String) -> Event a -> Event a
traceEventWith showA (Event f) = Event \e c ->
  f e (c . (\x -> trace (showA x) x))

-- | Print a debug message when value inside the Dynamic changes
traceDyn :: Show a => String -> Dynamic a -> Dynamic a
traceDyn tag = traceDynWith (((tag <> ": ") <>) . show)

-- | Print a debug message when value inside Dynamic changes using
-- given printing function
traceDynWith :: (a -> String) -> Dynamic a -> Dynamic a
traceDynWith showA d = d {dynamic_updates = e} where
  e = traceEventWith showA (dynamic_updates d)

-- | Print a debug message when value inside the DynRef changes
traceRef :: Show a => String -> DynRef a -> DynRef a
traceRef tag DynRef{..} = DynRef
  {dynref_dynamic = traceDynWith (((tag <> ": ") <>) . show) dynref_dynamic, ..}

-- | Print a debug message when value inside the DynRef changes
traceRefWith :: (a -> String) -> DynRef a -> DynRef a
traceRefWith f DynRef{..} = DynRef
  {dynref_dynamic = traceDynWith f dynref_dynamic, ..}

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event
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
  eventId <- EventId <$> liftIO (nextIntId renv)
  let
    updates = Event $ subscribeImpl eventId
    fire = defer eventId do
      newB <- liftIO $ fun <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      triggerImpl eventId renv newB
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
mapDyn2 fun adyn bdyn = do
  unsafeMapDynN
    (\[a, b] -> return $ fun (unsafeCoerce a) (unsafeCoerce b))
    [unsafeCoerce adyn, unsafeCoerce bdyn]

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
mapDyn3 fun adyn bdyn cdyn = do
  unsafeMapDynN
    (\[a, b, c] -> return $ fun (unsafeCoerce a) (unsafeCoerce b)
      (unsafeCoerce c))
    [unsafeCoerce adyn, unsafeCoerce bdyn, unsafeCoerce cdyn]

-- | Receives a list of Dynamics and a function to construct the
-- output. Position of elements from the list of [Any] that
-- function receives always correspond to positions of [Dynamic Any]
-- from which these values were generated. Dynamic created by this
-- function will only fire at most once per transaction and only in
-- case any of the input Dynamics change their values
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
  eventId <- EventId <$> liftIO (nextIntId renv)
  let
    fire = defer eventId do
      newOutput <- liftIO $ fun =<< readIORef latestInputsRef
      liftIO $ writeIORef latestOutputRef newOutput
      triggerImpl eventId renv newOutput
    updates = Event $ subscribeImpl eventId
    updateList _ _ [] = []
    updateList 0 a (_:xs) = a:xs
    updateList n a (x:xs) = x : updateList (pred n) a xs
  forM_ (zip [0..] dyns) \(i::Int, adyn) -> do
    dynamic_updates adyn `subscribe` \newVal -> do
      liftIO $ modifyIORef latestInputsRef $ updateList i newVal
      defer eventId fire
  return $ Dynamic (readIORef latestOutputRef) updates

runReactiveT :: ReactiveT m a -> ReactiveEnv -> m a
runReactiveT r = runReaderT (unReactiveT r)

execReactiveT :: ReactiveEnv -> ReactiveT m a -> m a
execReactiveT = flip runReactiveT

-- | Read and increment 'renv_id_generator'
nextIntId :: ReactiveEnv -> IO Int
nextIntId ReactiveEnv{..} = atomicModifyIORef'
  renv_id_generator \x -> (succ x, x)

-- | Defer a computation (usually an event firing) till the end of
-- current reactive transaction. This makes possible to avoid double
-- firing of events, constructed from multiple other events
defer :: EventId -> Step () -> Step ()
defer k act = Step (modify f) where
  f (TransactState s) = TransactState (M.insert k act s)

-- | Run a reactive transaction.
newStep :: MonadIO m => Step a -> m a
newStep act = liftIO $ loop (TransactState M.empty) act where
  loop :: TransactState -> Step a -> IO a
  loop rs (Step act) = do
    (r, newRs) <- runStateT act rs
    case popQueue newRs of
      (Just newAct, newerRs) -> r <$ loop newerRs newAct
      (Nothing, _newerRs) -> return r
  popQueue intact@(TransactState m) = case M.minViewWithKey m of
    Just ((_, act), rest) -> (Just act, TransactState rest)
    Nothing -> (Nothing, intact)

subscribeImpl :: EventId -> ReactiveEnv -> Callback a -> IO Canceller
subscribeImpl eventId e@ReactiveEnv{..} k = do
  subsId <- SubscriptionId <$> nextIntId e
  let newCancel = (subsId, k . unsafeCoerce)
  alterSubs $ Just . (newCancel :) . fromMaybe []
  return $ alterSubs $
    mfilter (not . Prelude.null) . Just . deleteSub subsId . fromMaybe []
  where
    alterSubs = modifyIORef' renv_subscriptions . flip M.alter eventId
    deleteSub _sId [] = []
    deleteSub sId ((xId, c):xs)
      | sId == xId = xs
      | otherwise = (xId, c) : deleteSub sId xs

triggerImpl :: EventId -> ReactiveEnv -> a -> Step ()
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

instance Applicative m => HasReactiveEnv (ReactiveT m) where
  askReactiveEnv = ReactiveT $ ReaderT pure
