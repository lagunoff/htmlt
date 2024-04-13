module Clickable.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.List qualified as List
import Data.Map qualified as Map
import Unsafe.Coerce

import Clickable.Protocol
import Clickable.Protocol.Value (Value)
import Clickable.Types

emptyState :: InternalState
emptyState = InternalState [] [] Map.empty [] 0

newInternalEnv :: (HaskellMessage -> IO JavaScriptMessage) -> IO InternalEnv
newInternalEnv send_message = do
  let scope = ResourceScope emptyState.next_id
  internal_state_ref <- newIORef emptyState {next_id = emptyState.next_id + 1}
  return InternalEnv {internal_state_ref, scope, send_message}

-- | Unsafe because there is no gurantee that @a@ matches @a@ in
-- correspoding @DynVar a@ where SourceId comes from
unsafeTrigger :: SourceId -> a -> InternalState -> InternalState
unsafeTrigger sourceId a =
  defer sourceId $ gets (.subscriptions) >>= notify
  where
    notify [] = return ()
    notify ((_, s, cb) : xs)
      | s == sourceId = cb (unsafeCoerce a) >> notify xs
      | otherwise = notify xs
    defer k act s = s {transaction_queue = Map.insert k act s.transaction_queue}

newScope :: ResourceScope -> InternalState -> (InternalState, ResourceScope)
newScope p s =
  let
    scopeId = ResourceScope s.next_id
    finalizers = (p, ScopeFinalizer scopeId) : s.finalizers
    s' = s {finalizers, next_id = s.next_id + 1}
  in
    (s', scopeId)

newVarId :: ResourceScope -> InternalState -> (InternalState, VarId)
newVarId e s =
  (s {next_id = s.next_id + 1}, VarId e s.next_id)

freeScope ::
  Bool ->
  ResourceScope ->
  InternalState -> (InternalState, [(ResourceScope, FinalizerVal)])
freeScope unlink rscope s =
  let
    chkSub (s, _, _) = s /= rscope
    chkFin True (s1, ScopeFinalizer s2) = s1 /= rscope && s2 /= rscope
    chkFin True (s, _) = s /= rscope
    chkFin False (s, _) = s /= rscope
    (finalizers, scopeFns) = List.partition (chkFin unlink) s.finalizers
    subscriptions = List.filter chkSub s.subscriptions
  in
    (s {subscriptions, finalizers}, scopeFns)

installFinalizer :: ClickM () -> ResourceScope -> InternalState -> InternalState
installFinalizer k scope s = s
  {finalizers = (scope, CustomFinalizer k) : s.finalizers}

subscribe :: DynVal a -> (a -> ClickM ()) -> ClickM ()
subscribe (ConstVal _) _ = return ()
subscribe (FromVar (SourceVar srcid _)) k = reactive_ g where
  g scope s = s {subscriptions = (scope, srcid, k . unsafeCoerce) : s.subscriptions }
subscribe (FromVar (OverrideVar _ var)) k =
  subscribe (FromVar var) k
subscribe (FromVar (LensMap l var)) k =
  subscribe (FromVar var) (k . getConst . l Const)
subscribe (MapVal v f) k = subscribe v (k . f)
subscribe (MapHoldVal _ _ srcid ref) k = reactive_ g where
  g scope s = s {subscriptions = (scope, srcid, k . unsafeCoerce) : s.subscriptions }
subscribe (SplatVal fv av) k = do
  src <- reactive h
  subscribe fv $ f src
  subscribe av $ g src
  where
    h scope s = (s', SourceId s.next_id) where
      s' = s
        { subscriptions = newsub : s.subscriptions
        , next_id = s.next_id + 1
        }
      newsub = (scope, SourceId s.next_id, k . unsafeCoerce)
    f src fv' = do
      av' <- readVal av
      modify $ unsafeTrigger src $ fv' av'
    g src av' = do
      fv' <- readVal fv
      modify $ unsafeTrigger src $ fv' av'
subscribe (OverrideSub f a) k = f (subscribe a) k
subscribe (FoldVal f a b srcid ref) k = reactive_ g where
  g scope s = s {subscriptions = (scope, srcid, k . unsafeCoerce) : s.subscriptions }

readVal :: MonadIO m => DynVal a -> m a
readVal (ConstVal a) = pure a
readVal (FromVar var) = readVar var
readVal (MapVal val f) = fmap f $ readVal val
readVal (MapHoldVal _ _ _ ref) = liftIO $ readIORef ref
readVal (SplatVal f a) = liftA2 ($) (readVal f) (readVal a)
readVal (OverrideSub _ a) = readVal a
readVal (FoldVal _ _ _ _ ref) = liftIO $ readIORef ref

readVar :: MonadIO m => DynVar a -> m a
readVar (SourceVar _ ref) = liftIO $ readIORef ref
readVar (LensMap l var) = fmap (getConst . l Const) $ readVar var
readVar (OverrideVar _ var) = readVar var

newCallback ::
  (Value -> ClickM ()) ->
  ResourceScope ->
  InternalState -> (InternalState, SourceId)
newCallback k rscope s =
  let
    new = (rscope, SourceId s.next_id, k . unsafeCoerce)
    subscriptions = new : s.subscriptions
  in
    (s { next_id = s.next_id + 1, subscriptions}, SourceId s.next_id)

launchClickM :: InternalEnv -> ClickM a -> IO a
launchClickM env = flip runReaderT env . unClickT . (<* syncPoint) . trampoline

-- | Loop until transaction_queue is empty.
--
-- This makes possible to implement @Applicative DynVal@ without
-- redundantly firing callback for the final result. TODO: Is this
-- even worth-while to have?  What if just let multiple DOM changes
-- when it depends on multiple sources?
trampoline :: ClickM a -> ClickM a
trampoline act = loop0 act where
  loop0 :: ClickM a -> ClickM a
  loop0 before = do
    r <- before
    mcont <- popQueue
    forM_ mcont loop1
    return r
  loop1 :: ClickM () -> ClickM ()
  loop1 before = do
    before
    mcont <- popQueue
    forM_ mcont loop1
  popQueue :: ClickM (Maybe (ClickM ()))
  popQueue = state \s ->
    case Map.minViewWithKey s.transaction_queue of
      Nothing -> (Nothing, s)
      Just ((_, r), newQueue) -> (Just r, s {transaction_queue = newQueue})

syncPoint :: ClickM ()
syncPoint = do
  send_message <- asks (.send_message)
  queue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  unless (List.null queue) do
    liftIO $ send_message $ EvalExpr $ RevSeq queue
    return ()

reactive :: (ResourceScope -> InternalState -> (InternalState, a)) -> ClickM a
reactive f = ClickT $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ f e.scope

reactive_ :: (ResourceScope -> InternalState -> InternalState) -> ClickM ()
reactive_ f = reactive \scope s -> (f scope s, ())

data ClientMessage
  = BrowserMessage JavaScriptMessage
  -- ^ Regular command received from JavaScript environment
  | DevServerMessage (ClickM ())
  -- ^ Bypass protocol and inject a command directly into the
  -- DevServer instance (useful for delivering notifications under
  -- devserver)

mapHoldVal :: (a -> b) -> DynVal a -> ClickM (DynVal b)
mapHoldVal f da = do
  a <- readVal da
  ref <- liftIO $ newIORef $ f a
  reactive $ g ref
  where
    g ref scope s = (s', val) where
      srcId = SourceId s.next_id
      newSub = (scope, srcId, k . unsafeCoerce)
      k a = do
        let b = f a
        liftIO $ writeIORef ref b
        modify $ unsafeTrigger srcId b
      s' = s {subscriptions = newSub : s.subscriptions, next_id = succ s.next_id}
      val = MapHoldVal da f srcId ref

foldVal :: (a -> b -> b) -> b -> DynVal a -> ClickM (DynVal b)
foldVal f b da = do
  ref <- liftIO $ newIORef b
  reactive $ g ref
  where
    g ref scope s = (s', val) where
      srcId = SourceId s.next_id
      newSub = (scope, srcId, k . unsafeCoerce)
      k a = do
        b <- liftIO $ readIORef ref
        let b' = f a b
        liftIO $ writeIORef ref b'
        modify $ unsafeTrigger srcId b'
      s' = s {subscriptions = newSub : s.subscriptions, next_id = succ s.next_id}
      val = FoldVal f b da srcId ref
