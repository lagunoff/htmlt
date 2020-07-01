{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Massaraksh.Event where

import Control.Applicative
import Control.Monad.State
import Control.Lens (Lens', over)
import Data.Foldable
import Data.IORef
import Data.List
import qualified Data.Map as M
import Debug.Trace
import System.IO.Unsafe

-- | @Event a@ is a stream of event occurences of type @a@
newtype Event a = Event (Stage -> Callback a -> Reactive Canceller)

data Stage = Immediate | Defer
  deriving (Show, Eq, Ord)

data Dynamic a = Dynamic
  { dyn_read    :: IO a    -- ^ Read current value
  , dyn_updates :: Event a -- ^ Event that fires when the value changes
  }

type Callback a = a -> Reactive ()
type Trigger a = a -> Reactive ()
type Modifier a = (a -> a) -> Reactive ()
type Canceller = IO ()
type DynamicRef a = (Dynamic a, Modifier a)
type ActId = Int

-- | Create new event and a function to supply values to that event
newEvent :: IO (Event a, Trigger a)
newEvent = do
  immediateSubs <- newIORef []
  deferredSubs <- newIORef []
  actId <- liftIO newActId
  let
    event = Event \fs k -> do
      kRef <- liftIO (newIORef k) -- Need 'IORef' for an 'Eq' instance
      let ref = case fs of Immediate -> immediateSubs; Defer -> deferredSubs
      liftIO $ modifyIORef ref ((:) kRef)
      pure $ liftIO $ modifyIORef ref (delete kRef)
    trigger = \a -> do
      fire a immediateSubs
      defer actId do fire a deferredSubs
    fire a ref = do
      callbacks <- liftIO (readIORef ref)
      for_ callbacks $ liftIO . readIORef >=> ($ a)
  pure (event, trigger)

actIdSupply :: IORef ActId
actIdSupply = unsafePerformIO (newIORef 0)

newActId :: IO ActId
newActId = atomicModifyIORef actIdSupply \x -> (succ x, succ x)

newtype Reactive a = Reactive (StateT ReactiveState IO a)
  deriving stock Functor
  deriving newtype (Applicative, Monad, MonadIO, MonadFix)

instance Semigroup x => Semigroup (Reactive x) where
  (<>) = liftA2 (<>)

instance Monoid x => Monoid (Reactive x) where
  mempty = pure mempty

class MonadReactive m where
  liftReactive :: Reactive x -> m x

defer :: Int -> Reactive () -> Reactive ()
defer k act = Reactive do
  modify \(ReactiveState s) -> ReactiveState (M.insert k act s)

newtype ReactiveState = ReactiveState
  { rs_deferred_acts :: M.Map ActId (Reactive ())
  }

instance Semigroup ReactiveState where
  (<>) (ReactiveState a) (ReactiveState b) = ReactiveState (a <> b)

instance Monoid ReactiveState where
  mempty = ReactiveState mempty

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

subscribe :: Event a -> Callback a -> Reactive Canceller
subscribe (Event s) = s Defer
{-# INLINE subscribe #-}

subscribeImmediate :: Event a -> Callback a -> Reactive Canceller
subscribeImmediate (Event s) = s Immediate
{-# INLINE subscribeImmediate #-}

-- | Filter and map occurences
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f e = Event \s k -> subscribe e $ maybe mempty k . f
{-# INLINE mapMaybeE #-}

-- | Filter and map occurences with side effects
mapMaybeE' :: (a -> IO (Maybe b)) -> Event a -> Event b
mapMaybeE' f (Event e) = Event \s k ->
  e s \a -> maybe mempty k =<< liftIO (f a)
{-# INLINE mapMaybeE' #-}

never :: Event a
never = Event \_ -> mempty

-- | Create new 'Dynamic' and a function to update the value
newDyn :: a -> IO (Dynamic a, Modifier a)
newDyn initial = do
  ref <- newIORef initial
  (ev, push) <- newEvent
  let
    modify = \f -> do
      old <- liftIO (readIORef ref)
      let new = f old
      liftIO (writeIORef ref new)
      push new
  pure (Dynamic (readIORef ref) ev, modify)

mapMaybeD :: b -> (a -> Maybe b) -> Dynamic a -> IO (Dynamic b)
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

constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

lensMap :: Lens' s a -> DynamicRef s -> DynamicRef a
lensMap stab (d, m) = (Dynamic read upd, modify) where
  read   = fmap (getConst . stab Const) $ dyn_read d
  upd    = fmap (getConst . stab Const) $ dyn_updates d
  modify = \f -> m (over stab f)

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
traceDynWith show' tag d = d {dyn_updates = e} where
  e = traceEventWith show' tag (dyn_updates d)
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
      k a *> mempty
    Defer     -> \k -> do
      actId <- liftIO newActId
      defer actId (k a)
      mempty
  (<*>) eF eA = Event \s k -> do
    latestF <- liftIO (newIORef Nothing)
    latestA <- liftIO (newIORef Nothing)
    actId <- liftIO newActId
    let
      doFire = do
        f <- liftIO $ readIORef latestF
        a <- liftIO $ readIORef latestA
        for_ (liftA2 (,) f a) (k . uncurry ($))
      fire = case s of Immediate -> doFire; Defer -> defer actId doFire
    c1 <- eF `subscribeImmediate` \f -> do
      liftIO $ writeIORef latestF (Just f)
      fire
    c2 <- eA `subscribeImmediate` \a -> do
      liftIO $ writeIORef latestA (Just a)
      fire
    pure (c1 *> c2)

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) df da = Dynamic r u where
    r = liftA2 ($) (dyn_read df) (dyn_read da)
    u = Event \s k -> do
      actId <- liftIO newActId
      let
        doFire newF newA = do
          f <- liftIO $ maybe (dyn_read df) pure newF
          a <- liftIO $ maybe (dyn_read da) pure newA
          k (f a)
        fire newF newA = case s of
          Immediate -> doFire newF newA
          Defer     -> defer actId (doFire newF newA)
      c1 <- dyn_updates df `subscribeImmediate` \f ->
        fire (Just f) Nothing
      c2 <- dyn_updates da `subscribeImmediate` \a ->
        fire Nothing (Just a)
      pure (c1 *> c2)

(<**>) :: DynamicRef a -> DynamicRef b -> DynamicRef (a, b)
(<**>) (dA, aMod) (bDyn, bMod) = (dyn, mod) where
  dyn = (,) <$> dA <*> bDyn
  mod = \f -> do
    oldA <- liftIO $ dyn_read dA
    oldB <- liftIO $ dyn_read bDyn
    let (newA, newB) = f (oldA, oldB)
    aMod \_ -> newA
    bMod \_ -> newB
