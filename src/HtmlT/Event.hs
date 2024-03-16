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
import Data.List qualified as List
import Data.Function

data ReactiveEnv = ReactiveEnv
  { scope :: ReactiveScope
  , reactive_state_ref :: IORef ReactiveState
  } deriving (Generic)

newReactiveEnv :: IO ReactiveEnv
newReactiveEnv = do
  let scope = ReactiveScope emptyReactiveState.id_supply
  reactive_state_ref <- newIORef emptyReactiveState
  return ReactiveEnv {reactive_state_ref, scope}

data ReactiveState = ReactiveState
  { subscriptions :: Map EventId [(ReactiveScope, Any -> RI ())]
  , scopes :: Map ReactiveScope ReactiveNode
  , transaction_queue :: Map EventId (RI ())
  , id_supply :: Int
  -- ^ Id supply for EventId and ReactiveScope
  } deriving (Generic)

emptyReactiveState :: ReactiveState
emptyReactiveState = ReactiveState
  { subscriptions = Map.empty
  , scopes = Map.empty
  , transaction_queue = Map.empty
  , id_supply = 0
  }

newtype EventId = EventId { unEventId :: Int }
  deriving newtype (Eq, Ord, Show, Num, Enum)

newtype ReactiveScope = ReactiveScope { unReactiveScope :: Int }
  deriving newtype (Show, Num, Ord, Eq)

-- | Represents a stream of event occurrences of type @a@. Its actual
-- representation is simply a function that subscribes to the event
newtype Event a = Event { subscribe :: (a -> RI ()) -> RI () }

instance Functor Event where
  fmap f (Event s) = Event \k -> s . (. f) $ k

-- | Represens a value that changes over time. Provides operations for
-- sampling the current value — 'readDyn' and for listening to future
-- changes 'updates'.
data Dynamic a = Dynamic
  { sample :: IO a
  -- ^ Read current value held by Dynamic
  , updates :: Event a
  -- ^ Event fires every time value changes, carries the new value of
  -- the Dynamic
  } deriving stock Generic

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) = dynamicSplat

dynamicSplat :: Dynamic (a -> b) -> Dynamic a -> Dynamic b
dynamicSplat df da =
  let
    updates = Event \k -> mdo
      let
        fire newF newA = defer queueId do
          f <- liftIO $ maybe (readDyn df) pure newF
          a <- liftIO $ maybe (readDyn da) pure newA
          k (f a)
      df.updates.subscribe \f -> fire (Just f) Nothing
      da.updates.subscribe \a -> fire Nothing (Just a)
      queueId <- EventId <$> reactive (const nextIntId)
      return ()
    sample = liftA2 ($) df.sample da.sample
  in
    Dynamic {sample, updates}

-- | A mutable variable that allows for subscription to new values. It
-- shares a similar API to 'IORef' (see 'readRef', 'writeRef',
-- 'modifyRef')
data DynRef a = DynRef
  { dynamic :: Dynamic a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , modifier :: Modifier a
  -- ^ Funtion to update the value
  } deriving stock Generic

-- | Function that updates the value inside the 'DynRef'
-- 'Bool' argument controls whether the modification should
-- trigger an update event. It's possible to update the 'DynRef'
-- without notifying the subscribers for optimization purposes, in
-- cases when you know that all changes already been reflected in
-- the DOM
newtype Modifier a = Modifier
  { unModifier :: forall r. Bool -> (a -> (a, r)) -> RI r
  }

newtype ReactiveT m a = ReactiveT
  { unReactiveT :: ReaderT ReactiveEnv m a
  }
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadFix, MonadCatch, MonadThrow,
    MonadMask, MonadReader ReactiveEnv
  )

instance MonadIO m => MonadReactive (ReactiveT m) where
  reactive f = ReactiveT $ ReaderT $ \e ->
    liftIO $ atomicModifyIORef' e.reactive_state_ref $ f e.scope
  {-# INLINE reactive #-}

type RI = ReactiveT IO

type Callback a = a -> RI ()

type Subscriptions = Map EventId [(ReactiveScope, Any -> RI ())]

type Finalizers = Map ReactiveScope ReactiveNode

data ReactiveNode = ReactiveNode
  { nodes :: [ReactiveScope]
  , parent :: Maybe ReactiveScope
  , finalizers :: [RI ()]
  } deriving stock Generic

freeScope :: ReactiveScope -> RI ()
freeScope rscope = do
  mRemovedNode <- reactive $ const $ freeScopeFn rscope
  forM_ mRemovedNode \n -> do
    sequence_ n.finalizers
    forM_ n.nodes freeScope

freeScopeFn :: ReactiveScope -> ReactiveState -> (ReactiveState, Maybe ReactiveNode)
freeScopeFn rscope s =
  let
    (mRemovedNode, scopes0) = Map.alterF (,Nothing) rscope $ s.scopes
    removedParent = mRemovedNode >>= (.parent)
    subscriptions = unsubscribe s.subscriptions
    scopes = maybe scopes0 (unlinkParentScope scopes0) removedParent
  in
    ((s::ReactiveState) { subscriptions, scopes }, mRemovedNode)
  where
    unsubscribe :: Subscriptions -> Subscriptions
    unsubscribe s =
      let
        removeCollectEmpty a k b =
          let c = filterSubscriptions b
          in  (if List.null c then k:a else a, c)
        (emptyKeys, s1) = Map.mapAccumWithKey removeCollectEmpty [] s
      in
        List.foldl' (flip Map.delete) s1 emptyKeys

    unlinkParentScope :: Finalizers -> ReactiveScope -> Finalizers
    unlinkParentScope f r =
      Map.alter (fmap (\n -> n { nodes = List.filter (/=rscope) n.nodes })) r f

    filterSubscriptions [] = []
    filterSubscriptions ((s, c):xs) | s == rscope = xs
                                    | otherwise = (s, c) : filterSubscriptions xs

unsafeSubscribeFn :: EventId -> (a -> RI ()) -> ReactiveScope -> ReactiveState -> ReactiveState
unsafeSubscribeFn eid k rs s =
  let
    insertSubscription (Just xs) = Just $ (rs, k . unsafeCoerce):xs
    insertSubscription Nothing = Just [(rs, k . unsafeCoerce)]
  in
    s {subscriptions = Map.alter insertSubscription eid s.subscriptions }

unsafeSubscribe :: EventId -> (a -> RI ()) -> RI ()
unsafeSubscribe eid k = reactive_ (unsafeSubscribeFn eid k)

installFinalizerFn :: RI () -> ReactiveScope -> ReactiveState -> ReactiveState
installFinalizerFn fin rs s =
  let
    insertFin (Just n) = Just $ n { finalizers = fin : n.finalizers}
    insertFin Nothing = Nothing
  in
    s {scopes = Map.alter insertFin rs s.scopes }

installFinalizer :: RI () -> RI ()
installFinalizer fin = reactive_ (installFinalizerFn fin)

unsafeTrigger :: EventId -> a -> RI ()
unsafeTrigger eid a = do
  defer eid do
    callbacks <- reactive \_ s ->
      (s, s.subscriptions & Map.lookup eid & fromMaybe [])
    forM_ callbacks $ ($ unsafeCoerce @_ @Any a) . snd

newReactiveScopeFn :: ReactiveScope -> ReactiveState -> (ReactiveState, ReactiveScope)
newReactiveScopeFn parent s0 =
  let
    (s1, intId) = nextIntId s0
    rscope = ReactiveScope intId
    scopes = Map.insert rscope (ReactiveNode [] (Just parent) []) s1.scopes
  in
    (s1 {scopes}, rscope)

newReactiveScope :: RI ReactiveScope
newReactiveScope = reactive newReactiveScopeFn

-- | Defers a computation (typically an event firing) until the end of
-- the current reactive transaction. This allows for the avoidance of
-- double firing of events constructed from multiple other events.
defer :: EventId -> RI () -> RI ()
defer k act = reactive_ \_ s -> s {transaction_queue = Map.insert k act s.transaction_queue}

newEvent :: RI (Event a, a -> RI ())
newEvent = reactive \_ s0 ->
  let
    (s1, eventId) = nextIntId s0
    event = Event $ unsafeSubscribe $ EventId eventId
    trig = unsafeTrigger $ EventId eventId
  in
    (s1, (event, trig))

newRef :: a -> RI (DynRef a)
newRef initial = do
  ioRef <- liftIO $ newIORef initial
  (event, push) <- newEvent
  let
    modifier = Modifier \u f -> do
      (new, result) <- liftIO $ atomicModifyIORef' ioRef \old ->
        let (new, result) = f old in
          (new, (new, result))
      when u $ push new
      return result
    dynamic = Dynamic (readIORef ioRef) event
  return DynRef {dynamic, modifier}

-- | Create a Dynamic that never changes its value
constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

-- | Event that will never fire
never :: Event a
never = Event \ _ -> return ()

-- | Write new value into a 'DynRef'
--
-- > ref <- newRef "Initial dynamic"
-- > transactionWrite ref "New dynamic"
-- > readRef ref
-- "New dynamic"
writeRef :: DynRef a -> a -> RI ()
writeRef ref a = modifyRef ref (const a)

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . (.dynamic)

-- | Update a 'DynRef' by applying given function to the current value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: DynRef a -> (a -> a) -> RI ()
modifyRef (DynRef _ (Modifier mod)) f = mod True $ (,()) . f

-- | Update a 'DynRef' with first field of the tuple and return back
-- the second field. The name is intended to be similar to
-- 'atomicModifyIORef' but there are no atomicity guarantees
-- whatsoever
atomicModifyRef :: DynRef a -> (a -> (a, r)) -> RI r
atomicModifyRef (DynRef _ (Modifier mod)) f = mod True f

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = (.dynamic)

-- | Read the dynamic held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . (.sample)

-- | Executes an action currently held inside the 'Dynamic' and every
-- time the dynamic changes.
performDyn :: Dynamic (RI ()) -> RI ()
performDyn d = do
  join $ liftIO d.sample
  d.updates.subscribe id

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equalFn da =
  let
    updates = Event \k -> do
      old <- liftIO da.sample
      oldRef <- liftIO $ newIORef old
      da.updates.subscribe \new -> do
        old <- liftIO $ atomicModifyIORef' oldRef (new,)
        unless (old `equalFn` new) $ k new
  in
    Dynamic {sample = da.sample, updates}

-- | Produce a new Dynamic by applying a function to both the source
-- (Dynamic a) and previous value of itself
foldDynMaybe :: (a -> b -> Maybe b) -> b -> Dynamic a -> RI (Dynamic b)
foldDynMaybe f initB dynA = do
  initA <- liftIO dynA.sample
  refB <- newRef $ fromMaybe initB $ f initA initB
  dynA.updates.subscribe \newA -> do
    oldB <- liftIO refB.dynamic.sample
    forM_ (f newA oldB) $ writeRef refB
  return refB.dynamic

nextIntId :: ReactiveState -> (ReactiveState, Int)
nextIntId s = (s {id_supply = succ s.id_supply}, s.id_supply)

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event. As
-- a general guideline, if the function @f@ is inexpensive, choose
-- @fmap f@. Otherwise, consider using @mapDyn f@.
mapDyn :: (a -> b) -> Dynamic a -> RI (Dynamic b)
mapDyn fun da = do
  initialA <- liftIO $ da.sample
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef (fun initialA)
  eventId <- EventId <$> reactive (const nextIntId)
  let
    updates = Event (unsafeSubscribe eventId)
    fire = defer eventId do
      newB <- liftIO $ fun <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      unsafeTrigger eventId newB
  da.updates.subscribe \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  return $ Dynamic (readIORef latestB) updates

mapDyn2 :: (a -> b -> c) -> Dynamic a -> Dynamic b -> RI (Dynamic c)
mapDyn2 fun da db = do
  initialA <- liftIO $ da.sample
  initialB <- liftIO $ db.sample
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef initialB
  latestC <- liftIO $ newIORef (fun initialA initialB)
  eventId <- EventId <$> reactive (const nextIntId)
  let
    updates = Event (unsafeSubscribe eventId)
    fire = defer eventId do
      newC <- liftIO $ fun <$> (readIORef latestA) <*> (readIORef latestB)
      liftIO $ writeIORef latestC newC
      unsafeTrigger eventId newC
  da.updates.subscribe \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  db.updates.subscribe \newB -> do
    liftIO $ writeIORef latestB newB
    defer eventId fire
  return $ Dynamic (readIORef latestC) updates

-- | Takes a list of Dynamics and a function to generate the
-- output. The positions of elements in the list of [Any] received by
-- the function always correspond to the positions of [Dynamic Any]
-- from which these values were generated. The Dynamic created by this
-- function will fire at most once per transaction, and only if any of
-- the input Dynamics change their values.
unsafeMapDynN
  :: ([Any] -> IO a)
  -- ^ Construct the output value, from list of input values from
  -- corresponding positions of given Dynamics
  -> [Dynamic Any]
  -- ^ List of input Dynamics
  -> RI (Dynamic a)
unsafeMapDynN fun dyns = do
  initialInputs <- liftIO $ mapM (.sample) dyns
  initialOutput <- liftIO $ fun initialInputs
  latestInputsRef <- liftIO $ newIORef initialInputs
  latestOutputRef <- liftIO $ newIORef initialOutput
  eventId <- EventId <$> reactive (const nextIntId)
  let
    fire = defer eventId do
      newOutput <- liftIO $ fun =<< readIORef latestInputsRef
      liftIO $ writeIORef latestOutputRef newOutput
      unsafeTrigger eventId newOutput
    updates = Event $ unsafeSubscribe eventId
    updateList _ _ [] = []
    updateList 0 a (_:xs) = a:xs
    updateList n a (x:xs) = x : updateList (pred n) a xs
  forM_ (zip [0..] dyns) \(i::Int, adyn) -> do
    adyn.updates.subscribe \newVal -> do
      liftIO $ modifyIORef latestInputsRef $ updateList i newVal
      defer eventId fire
  return $ Dynamic (readIORef latestOutputRef) updates

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Apply a lens to the value inside 'DynRef'
lensMap :: forall s a. Lens' s a -> DynRef s -> DynRef a
lensMap l s =
  let
    dynamic = Dynamic
      (fmap (getConst . l Const) s.dynamic.sample)
      (fmap (getConst . l Const) s.dynamic.updates)
    modifier = Modifier \u f ->
      unModifier s.modifier u $ swap . l (swap . f)
  in
    DynRef {dynamic, modifier}

-- | Loop until transaction_queue is empty
trampoline :: RI a -> RI a
trampoline act = loop0 act where
  loop0 :: RI a -> RI a
  loop0 before = do
    r <- before
    mcont <- popQueue
    forM_ mcont loop1
    return r
  loop1 :: RI () -> RI ()
  loop1 before = do
    before
    mcont <- popQueue
    forM_ mcont loop1
  popQueue :: RI (Maybe (RI ()))
  popQueue = reactive \_ s ->
    case Map.minViewWithKey s.transaction_queue of
      Nothing -> (s, Nothing)
      Just ((_, r), newQueue) -> (s {transaction_queue = newQueue}, Just r)

launchReactiveT :: ReactiveEnv -> RI a -> IO a
launchReactiveT e = execReactiveT e . trampoline

execReactiveT :: ReactiveEnv -> ReactiveT m a -> m a
execReactiveT e = flip runReaderT e . unReactiveT

runReactiveT :: ReactiveT m a -> ReactiveEnv -> m a
runReactiveT = flip execReactiveT

class MonadReactive m where
  reactive :: (ReactiveScope -> ReactiveState -> (ReactiveState, a)) -> m a

reactive_ :: MonadReactive m => (ReactiveScope -> ReactiveState -> ReactiveState) -> m ()
reactive_ f = reactive (\r s -> (,()) $ f r s)
