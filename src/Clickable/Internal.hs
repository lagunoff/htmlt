module Clickable.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Text (Text)
import Data.List qualified as List
import Data.Map qualified as Map
import Unsafe.Coerce
import GHC.Exts

import Clickable.Protocol
import Clickable.Protocol.Value (Value)
import Clickable.Types

emptyState :: InternalState
emptyState = InternalState [] [] Map.empty [] 0

newInternalEnv :: (Expr -> IO Value) -> IO InternalEnv
newInternalEnv eval_expr = do
  let scope = ResourceScope emptyState.next_id
  internal_state_ref <- newIORef emptyState {next_id = emptyState.next_id + 1}
  return InternalEnv {internal_state_ref, scope, eval_expr}

triggerEvent :: Event a -> a -> InternalState -> InternalState
triggerEvent eventId a =
  defer eventId $ gets (.subscriptions) >>= notify
  where
    anyEventId :: Event Any = coerce eventId
    notify :: [Subscription Any] -> ClickM ()
    notify [] = return ()
    notify (SubscriptionSimple {ss_event_id, ss_callback} : xs)
      | ss_event_id == anyEventId = ss_callback (unsafeCoerce a) >> notify xs
      | otherwise = notify xs
    notify (SubscriptionAccum {sa_event_id, sa_callback, sa_accum_ref} : xs)
      | sa_event_id == anyEventId = notifyAcc sa_callback sa_accum_ref >> notify xs
      | otherwise = notify xs
    notifyAcc :: forall b. (Any -> b -> ClickM b) -> IORef b ->  ClickM ()
    notifyAcc k ref = do
      acc <- liftIO $ readIORef ref
      acc' <- k (unsafeCoerce a) acc
      liftIO $ writeIORef ref acc'
    defer :: Event a -> ClickM () -> InternalState -> InternalState
    defer k act s = s {transaction_queue = Map.insert (coerce k) act s.transaction_queue}

newScope :: ResourceScope -> InternalState -> (InternalState, ResourceScope)
newScope p s =
  let
    scopeId = ResourceScope s.next_id
    finalizers = ScopeFinalizer p scopeId : s.finalizers
    s' = s {finalizers, next_id = s.next_id + 1}
  in
    (s', scopeId)

newVarId :: ResourceScope -> InternalState -> (InternalState, VarId)
newVarId e s = (s {next_id = s.next_id + 1}, VarId e s.next_id)

freeScope ::
  Bool ->
  ResourceScope ->
  InternalState -> (InternalState, [Finalizer])
freeScope unlink rscope s =
  let
    chkSub s = subscriptionScope s /= rscope
    chkFin True ScopeFinalizer{sf_resource_scope, sf_linked_scope} =
      sf_resource_scope /= rscope && sf_linked_scope /= rscope
    chkFin True CustomFinalizer{cf_resource_scope} = cf_resource_scope /= rscope
    chkFin False f = finalizerScope f /= rscope
    (finalizers, scopeFns) = List.partition (chkFin unlink) s.finalizers
    subscriptions = List.filter chkSub s.subscriptions
  in
    (s {subscriptions, finalizers}, scopeFns)

installFinalizer :: ClickM () -> ResourceScope -> InternalState -> InternalState
installFinalizer k scope s = s {finalizers = CustomFinalizer scope k : s.finalizers}

subscribe :: forall a. DynVal a -> (a -> ClickM ()) -> ClickM ()
subscribe (ConstVal _) _ = return ()
subscribe (FromVar (SourceVar srcid _)) k = reactive_ g where
  g scope s = s {subscriptions = SubscriptionSimple scope (coerce srcid) (k . unsafeCoerce) : s.subscriptions }
subscribe (FromVar (OverrideVar _ var)) k =
  subscribe (FromVar var) k
subscribe (FromVar (LensMap l var)) k =
  subscribe (FromVar var) (k . getConst . l Const)
subscribe (MapVal v f) k = subscribe v (k . f)
subscribe (MapHoldVal _ _ srcid ref) k = reactive_ g where
  g scope s = s {subscriptions = SubscriptionSimple scope (coerce srcid) (k . unsafeCoerce) : s.subscriptions }
subscribe (SplatVal fv av) k = do
  src <- reactive h
  subscribe fv $ f src
  subscribe av $ g src
  where
    h scope s = (s', coerce event) where
      s' = s
        { subscriptions = newsub : s.subscriptions
        , next_id = s.next_id + 1
        }
      event = unsafeFromEventId $ EventId s.next_id
      newsub = SubscriptionSimple scope event (k . unsafeCoerce)
    f src fv' = do
      av' <- readVal av
      modify $ triggerEvent src $ fv' av'
    g src av' = do
      fv' <- readVal fv
      modify $ triggerEvent src $ fv' av'
subscribe (OverrideSub f d) k = f subscribe' k' where
  k' a _ = k a
  subscribe' k = subscribe d \a -> k a ()

subscribeAccum :: DynVal a -> (a -> b -> ClickM b) -> b -> ClickM ()
subscribeAccum (ConstVal _) _ _ = return ()
subscribeAccum (FromVar (SourceVar srcid _)) k b = do
  ref <- liftIO $ newIORef b
  let newSub scope = SubscriptionAccum scope (coerce srcid) (k . unsafeCoerce) ref
  let g scope s = s {subscriptions = newSub scope : s.subscriptions}
  reactive_ g
subscribeAccum (FromVar (OverrideVar _ var)) k b =
  subscribeAccum (FromVar var) k b
subscribeAccum (FromVar (LensMap l var)) k b =
  subscribeAccum (FromVar var) (k . getConst . l Const) b
subscribeAccum (MapVal v f) k b = subscribeAccum v (k . f) b
subscribeAccum (MapHoldVal _ _ srcid ref) k b = do
  ref <- liftIO $ newIORef b
  let newSub scope = SubscriptionAccum scope (coerce srcid) (k . unsafeCoerce) ref
  let g scope s = s {subscriptions = newSub scope : s.subscriptions}
  reactive_ g
subscribeAccum (SplatVal fv av) k b = do
  ref <- liftIO $ newIORef b
  let
    h scope s = (s', coerce event) where
      s' = s
        { subscriptions = newsub : s.subscriptions
        , next_id = s.next_id + 1
        }
      event = unsafeFromEventId $ EventId s.next_id
      newsub = SubscriptionAccum scope event (k . unsafeCoerce) ref
    f src fv' = do
      av' <- readVal av
      modify $ triggerEvent src $ fv' av'
    g src av' = do
      fv' <- readVal fv
      modify $ triggerEvent src $ fv' av'
  src <- reactive h
  subscribe fv $ f src
  subscribe av $ g src
subscribeAccum (OverrideSub f a) k b = f (flip (subscribeAccum a) b) k

readVal :: MonadIO m => DynVal a -> m a
readVal (ConstVal a) = pure a
readVal (FromVar var) = readVar var
readVal (MapVal val f) = fmap f $ readVal val
readVal (MapHoldVal _ _ _ ref) = liftIO $ readIORef ref
readVal (SplatVal f a) = liftA2 ($) (readVal f) (readVal a)
readVal (OverrideSub _ a) = readVal a

readVar :: MonadIO m => DynVar a -> m a
readVar (SourceVar _ ref) = liftIO $ readIORef ref
readVar (LensMap l var) = fmap (getConst . l Const) $ readVar var
readVar (OverrideVar _ var) = readVar var

newCallback ::
  (a -> ClickM ()) ->
  ResourceScope ->
  InternalState -> (InternalState, Event a)
newCallback k rscope s =
  let
    event = unsafeFromEventId $ EventId s.next_id
    new = SubscriptionSimple rscope (coerce event) (k . unsafeCoerce)
    subscriptions = new : s.subscriptions
  in
    (s { next_id = s.next_id + 1, subscriptions}, event)

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
  eval_expr <- asks (.eval_expr)
  queue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  unless (List.null queue) do
    liftIO $ eval_expr $ RevSeq queue
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
      event = unsafeFromEventId $ EventId s.next_id
      newSub = SubscriptionSimple scope (coerce event) (k . unsafeCoerce)
      k a = do
        let b = f a
        liftIO $ writeIORef ref b
        modify $ triggerEvent event b
      s' = s {subscriptions = newSub : s.subscriptions, next_id = succ s.next_id}
      val = MapHoldVal da f event ref

unsafeInsertHtml :: Text -> Expr
unsafeInsertHtml rawHtml = Eval
  "(function(builder, rawHtml){\
   \var div = document.createElement('div');\
   \div.innerHTML = rawHtml;\
   \var iter = div.childNodes[0];\
   \for (; iter; iter = div.childNodes[0]) {\
   \  div.removeChild(iter);\
   \  if (builder instanceof Comment) {\
   \    builder.parentElement.insertBefore(iter, builder);\
   \  } else{\
   \    builder.appendChild(iter);\
   \  }\
   \}\
   \})" `Apply` [Arg 0 0, String rawHtml]
