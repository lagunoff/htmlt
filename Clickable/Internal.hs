{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Clickable.Internal where

import Clickable.Types
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Binary qualified as Binary
import Data.Binary.Put (execPut)
import Data.ByteString.Builder ( Builder )
import Data.ByteString.Builder.Extra (runBuilder, Next (..), BufferWriter)
import Data.ByteString.Unsafe
import Data.Functor.Const
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Word
import Foreign.C.String (CStringLen)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import GHC.Exts
import Unsafe.Coerce

newEvent :: ClickM (Event a)
newEvent = state \s ->
  (Event (EventId s.next_id), s {next_id = s.next_id + 1})

mapEvent :: (a -> b) -> Event a -> ClickM (Event b)
mapEvent f ea = do
  eb <- newEvent
  subscribeEvent ea $ triggerEvent eb . f
  return eb

subscribeEvent :: forall a. Event a -> (a -> ClickM ()) -> ClickM ()
subscribeEvent (Event eid) k = reactive_ g where
  newSub scope = SubscriptionSimple scope (coerce eid) (k . unsafeCoerce)
  g scope s = s {subscriptions = newSub scope : s.subscriptions}

subscribe :: forall a. DynVal a -> (a -> ClickM ()) -> ClickM ()
subscribe (ConstVal _) _ = return ()
subscribe (FromVar (SourceVar srcid _)) k = reactive_ g where
  newSub scope = SubscriptionSimple scope (coerce srcid) (k . unsafeCoerce)
  g scope s = s {subscriptions = newSub scope : s.subscriptions}
subscribe (FromVar (OverrideVar _ var)) k =
  subscribe (FromVar var) k
subscribe (FromVar (LensMap l var)) k =
  subscribe (FromVar var) (k . getConst . l Const)
subscribe (MapVal v f) k = subscribe v (k . f)
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
      triggerEvent src $ fv' av'
    g src av' = do
      fv' <- readVal fv
      triggerEvent src $ fv' av'
subscribe (OverrideSub f d) k = f subscribe' k' where
  k' a _ = k a
  subscribe' c = subscribe d \a -> c a ()

triggerEventOp :: Event a -> a -> InternalState -> InternalState
triggerEventOp event pload =
  defer event $ gets (.subscriptions) >>= notify
  where
    eventEq :: forall a b. Event a -> Event b -> Bool
    eventEq (Event a) (Event b) = a == b

    notify :: [Subscription Any] -> ClickM ()
    notify [] = return ()
    notify (SubscriptionSimple {ss_event_id, ss_callback} : xs)
      | ss_event_id `eventEq` event = ss_callback (unsafeCoerce pload) >> notify xs
      | otherwise = notify xs
    notify (SubscriptionAccum {sa_event_id, sa_callback, sa_accum_ref} : xs)
      | sa_event_id `eventEq` event = notifyAcc sa_callback sa_accum_ref >> notify xs
      | otherwise = notify xs
    notifyAcc :: forall b. (Any -> b -> ClickM b) -> IORef b ->  ClickM ()
    notifyAcc k ref = do
      acc <- liftIO $ readIORef ref
      acc' <- k (unsafeCoerce pload) acc
      liftIO $ writeIORef ref acc'
    defer :: Event a -> ClickM () -> InternalState -> InternalState
    defer k act s = s
      {transaction_queue = Map.insert k.unEvent act s.transaction_queue}

triggerEvent :: Event a -> a -> ClickM ()
triggerEvent e a = modify $ triggerEventOp e a
{-# INLINE triggerEvent #-}

reactive :: (ScopeId -> InternalState -> (InternalState, a)) -> ClickM a
reactive f = ClickM \e -> atomicModifyIORef' e.hte_state $ f e.hte_scope
{-# INLINE reactive #-}

reactive_ :: (ScopeId -> InternalState -> InternalState) -> ClickM ()
reactive_ f = reactive \scope s -> (f scope s, ())
{-# INLINE reactive_ #-}

-- | Loop until transaction_queue is empty.
--
-- Makes possible to implement @Applicative DynVal@ without invoking
-- subscribers redundantly when multiple events are fired in the same
-- transition
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

runTransition :: InternalEnv -> ClickM () -> IO ()
runTransition e c =
  prompt e.hte_prompt_tag $
    ($ e) . unClickM . (<* syncPoint) . trampoline $ c

syncPoint :: ClickM ()
syncPoint = ClickM \e -> void $ e.hte_flush

unsafeInsertHtml :: Text -> Expr
unsafeInsertHtml rawHtml = Eval
  "(function(parent, rawHtml){\
   \var div = document.createElement('div');\
   \div.innerHTML = rawHtml;\
   \var iter = div.childNodes[0];\
   \for (; iter; iter = div.childNodes[0]) {\
   \  div.removeChild(iter);\
   \  if (parent instanceof Comment) {\
   \    parent.parentElement.insertBefore(iter, parent);\
   \  } else{\
   \    parent.appendChild(iter);\
   \  }\
   \}\
   \})" `Apply` [PeekStack 0, Str rawHtml]

newScope :: ClickM ScopeId
newScope = reactive newScopeOp
{-# INLINE newScope #-}

newScopeOp :: ScopeId -> InternalState -> (InternalState, ScopeId)
newScopeOp p s = (s', scope)
  where
    s' = s {finalizers = fns, next_id = s.next_id + 1}
    fns = ScopeFinalizer p scope : s.finalizers
    scope = ScopeId s.next_id
{-# INLINE newScopeOp #-}

newRefId :: ClickM RefId
newRefId = reactive newRefIdOp
{-# INLINE newRefId #-}

newRefIdOp :: ScopeId -> InternalState -> (InternalState, RefId)
newRefIdOp e s = (s {next_id = s.next_id + 1}, RefId e s.next_id)
{-# INLINE newRefIdOp #-}

freeScope :: Bool -> ScopeId -> ClickM ()
freeScope unlink s =
  reactive (const (freeScopeOp unlink s)) >>= applyFin
  where
    applyFin [] = enqueueExpr $ FreeScope s
    applyFin (ScopeFinalizer{sf_linked_scope}:xs) = freeScope True sf_linked_scope >> applyFin xs
    applyFin (CustomFinalizer{cf_callback}:xs) = cf_callback >> applyFin xs
{-# INLINE freeScope #-}

freeScopeOp :: Bool -> ScopeId -> InternalState -> (InternalState, [Finalizer])
freeScopeOp unlink scope s =
  (s {subscriptions, finalizers}, scopeFns)
  where
    chkSub s' = subscriptionScope s' /= scope
    chkFin True ScopeFinalizer{sf_resource_scope, sf_linked_scope} =
      sf_resource_scope /= scope && sf_linked_scope /= scope
    chkFin True CustomFinalizer{cf_resource_scope} = cf_resource_scope /= scope
    chkFin False f = finalizerScope f /= scope
    (finalizers, scopeFns) = List.partition (chkFin unlink) s.finalizers
    subscriptions = List.filter chkSub s.subscriptions
{-# INLINE freeScopeOp #-}

installFinalizer :: ClickM () -> ClickM ()
installFinalizer = reactive_ . installFinalizerOp
{-# INLINE installFinalizer #-}

installFinalizerOp :: ClickM () -> ScopeId -> InternalState -> InternalState
installFinalizerOp k scope s =
  s {finalizers = CustomFinalizer scope k : s.finalizers}
{-# INLINE installFinalizerOp #-}

emptyState :: InternalState
emptyState = InternalState [] [] Map.empty 0

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

readVal :: MonadIO m => DynVal a -> m a
readVal (ConstVal a) = pure a
readVal (FromVar var) = readVar var
readVal (MapVal val f) = fmap f $ readVal val
readVal (SplatVal f a) = liftA2 ($) (readVal f) (readVal a)
readVal (OverrideSub _ a) = readVal a

readVar :: MonadIO m => DynVar a -> m a
readVar (SourceVar _ ref) = liftIO $ readIORef ref
readVar (LensMap l var) = fmap (getConst . l Const) $ readVar var
readVar (OverrideVar _ var) = readVar var

newVar :: a -> ClickM (DynVar a)
newVar a = do
  ref <- liftIO $ newIORef a
  let mkEv s = unsafeFromEventId $ EventId s.next_id
  state \s -> (SourceVar (mkEv s) ref, s {next_id = s.next_id + 1})

overrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
overrideVar = OverrideVar

lensMap :: Lens' s a -> DynVar s -> DynVar a
lensMap = LensMap

modifyVar :: DynVar s -> (s -> (s, a)) -> ClickM a
modifyVar (SourceVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  triggerEvent varId newVal
  return a
  where
    g old = let (new, a) = f old in (new, (new, a))
modifyVar (OverrideVar ufn var) f =
  ufn (modifyVar var) f
modifyVar (LensMap l var) f = modifyVar var (swap . l (swap . f))

modifyVar_ :: DynVar s -> (s -> s) -> ClickM ()
modifyVar_ var f = modifyVar var ((,()) . f)

writeVar :: DynVar s -> s -> ClickM ()
writeVar var s = modifyVar_ var $ const s

forDyn :: DynVal a -> (a -> ClickM ()) -> ClickM ()
forDyn dval action = readVal dval >>= action >> subscribe dval action

forVar :: DynVar a -> (a -> ClickM ()) -> ClickM ()
forVar = forDyn . fromVar

-- | Update the value inside a DynVar without notifying
-- subscribers. Intended to be used as a workaround to synchronize the
-- state when the DOM was already updated locally. Consider "input"
-- event on <input/> elements: in some cases you can use
-- modifyVarQuiet for efficiency since you already know <input/> value
-- has already been updated.
--
-- @
--   counter <- newVar (0::Int)
--   input_ do
--     dynProp "value" counter
--     on @"input" \t ->
--        forM_ (readMaybe t) \v -> writeVarQuiet_ counter v
--   button_ do {text "-"; on @"click" $ modifyVar_ counter pred;}
--   button_ do {text "+"; on @"click" $ modifyVar_ counter succ;}
-- @
modifyVarQuiet :: DynVar s -> (s -> (s, a)) -> ClickM a
modifyVarQuiet (SourceVar _varId ref) f = do
  liftIO $ atomicModifyIORef' ref f
modifyVarQuiet (OverrideVar ufn var) f =
  ufn (modifyVarQuiet var) f
modifyVarQuiet (LensMap l var) f =
  modifyVarQuiet var (swap . l (swap . f))
{-# INLINEABLE modifyVarQuiet #-}

modifyVarQuiet_ :: DynVar s -> (s -> s) -> ClickM ()
modifyVarQuiet_ var f = modifyVarQuiet var ((,()) . f)
{-# INLINE modifyVarQuiet_ #-}

writeVarQuiet :: DynVar s -> s -> ClickM ()
writeVarQuiet var = modifyVarQuiet_ var . const
{-# INLINE writeVarQuiet #-}

enqueueExpr :: Expr -> ClickM ()
enqueueExpr exp = ClickM \e ->
  e.hte_send $ execPut $ Binary.put exp
{-# INLINE enqueueExpr #-}

evalExpr :: Expr -> ClickM Expr
evalExpr exp = ClickM \e -> do
  e.hte_send $ execPut $ Binary.put exp
  return undefined
{-# INLINE evalExpr #-}

withBuffer :: Ptr Word8 -> Int -> (CStringLen -> IO ()) -> IO (Builder -> IO (), IO ())
withBuffer buf bufSize consume = do
  -- buf <- mallocBytes bufSize
  offset <- newIORef 0
  let write b = do
        off <- readIORef offset
        newOff <- writeLoop (runBuilder b) off
        writeIORef offset newOff
      writeLoop :: BufferWriter -> Int -> IO Int
      writeLoop bufWrite off = do
        (written, next) <- bufWrite (buf `plusPtr` off) (bufSize - off)
        case next of
          Done -> pure $ off + written
          More _size moreWrite -> do
            consume (castPtr buf, bufSize)
            writeLoop moreWrite 0
          Chunk chunk moreWrite -> do
            unsafeUseAsCStringLen chunk consume
            writeLoop moreWrite off
      flush = do
        off <- atomicModifyIORef' offset (0,)
        consume (castPtr buf, off)
  return (write, flush)

newInternalEnv :: Int -> (CStringLen -> IO ()) -> IO
  ( InternalEnv
  , IORef (Map Word32 (IO ValueExpr -> IO ()))
  , CStringLen )
newInternalEnv bufSize consume = do
  buf <- mallocBytes bufSize
  hte_state <- newIORef emptyState
  (send', flush) <- withBuffer buf bufSize consume
  hte_prompt_tag <- newPromptTag
  continuations <- newIORef Map.empty
  let env = InternalEnv
        { hte_send = send'
        , hte_flush = do
            tid <- atomicModifyIORef' hte_state \s ->
              (s {next_id = s.next_id + 1}, s.next_id)
            send' $ execPut $ Binary.put $ YieldResult tid
            flush
            control hte_prompt_tag \c ->
              modifyIORef' continuations $ Map.insert tid c
        , hte_state
        , hte_scope = ScopeId 0
        , hte_prompt_tag
        }
  pure (env, continuations, (castPtr buf, bufSize))
