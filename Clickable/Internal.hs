{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall #-}
module Clickable.Internal where

import Clickable.Types
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Binary qualified as Binary
import Data.Binary.Put (execPut)
import Data.ByteString.Builder.Extra (runBuilder, Next (..), BufferWriter)
import Data.Functor.Const
import Data.IORef
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Tuple (swap)
import Foreign.C.String (CStringLen)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import GHC.Exts
import Unsafe.Coerce
import Data.Maybe

newEvent :: JSM (Event a)
newEvent = state \s ->
  (Event (EventId s.ist_id_supply), s {ist_id_supply = s.ist_id_supply + 1})

mapEvent :: (a -> b) -> Event a -> JSM (Event b)
mapEvent f ea = do
  eb <- newEvent
  subscribeEvent ea $ triggerEvent eb . f
  return eb

mapMaybeEvent :: (a -> Maybe b) -> Event a -> JSM (Event b)
mapMaybeEvent f ea = do
  eb <- newEvent
  subscribeEvent ea $ mapM_ (triggerEvent eb) . f
  return eb

subscribeEvent :: forall a. Event a -> (a -> JSM ()) -> JSM ()
subscribeEvent e k = reactive_ $ subscribeEventFn e k

subscribeEventFn :: forall a. Event a -> (a -> JSM ()) -> ScopeId -> InternalState -> InternalState
subscribeEventFn (Event eid) k scope s =
  s {
    ist_subscriptions = Map.alter ins eid s.ist_subscriptions
  }
  where
    ins Nothing = Just [newSub]
    ins (Just xs) = Just $ newSub : xs
    newSub = Subscription scope (k . unsafeCoerce)

subscribeOnce :: forall a. Event a -> (a -> JSM ()) -> JSM ()
subscribeOnce e k = do
  scope <- newScope
  localScope scope do
    subscribeEvent e \pload -> do
      k pload
      destroyScope scope

subscribe :: forall a. Dynamic a -> (a -> JSM ()) -> JSM ()
subscribe (ConstVal _) _ = return ()
subscribe (FromVar (SourceVar event _)) k = reactive_ g where
  ins sub Nothing = Just [sub]
  ins sub (Just xs) = Just $ sub : xs
  newSub scope = Subscription scope (k . unsafeCoerce)
  g scope s = s {
    ist_subscriptions = Map.alter (ins (newSub scope)) event.unEvent s.ist_subscriptions
  }
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
    ins sub Nothing = Just [sub]
    ins sub (Just xs) = Just $ sub : xs
    h scope s = (s', coerce event) where
      s' = s {
        ist_subscriptions = Map.alter (ins newsub) event.unEvent s.ist_subscriptions,
        ist_id_supply = s.ist_id_supply + 1
      }
      event = unsafeFromEventId $ EventId s.ist_id_supply
      newsub = Subscription scope (k . unsafeCoerce)
    f src fv' = do
      av' <- readDyn av
      triggerEvent src $ fv' av'
    g src av' = do
      fv' <- readDyn fv
      triggerEvent src $ fv' av'
subscribe (OverrideSub f d) k = f subscribe' k' where
  k' a _ = k a
  subscribe' c = subscribe d \a -> c a ()

triggerEvent :: Event a -> a -> JSM ()
triggerEvent e a = modify $ triggerEventFn e a
{-# INLINE triggerEvent #-}

triggerEventFn :: Event a -> a -> InternalState -> InternalState
triggerEventFn event pload =
  defer event $ gets look >>= mapM_ notify
  where
    look = fromMaybe [] . Map.lookup event.unEvent . (.ist_subscriptions)
    notify s = s.sub_callback (unsafeCoerce pload)

    defer :: Event a -> JSM () -> InternalState -> InternalState
    defer k act s = s {
      ist_transaction_queue = Map.insert k.unEvent act s.ist_transaction_queue
    }

reactive :: (ScopeId -> InternalState -> (InternalState, a)) -> JSM a
reactive f = JSM \e -> atomicModifyIORef' e.ien_state $ f e.ien_scope
{-# INLINE reactive #-}

reactive_ :: (ScopeId -> InternalState -> InternalState) -> JSM ()
reactive_ f = reactive \scope s -> (f scope s, ())
{-# INLINE reactive_ #-}

-- | Loop until ist_transaction_queue is empty.
--
-- Makes possible to implement @Applicative Dynamic@ without invoking
-- subscribers redundantly when multiple events are fired in the same
-- transition
trampoline :: JSM a -> JSM a
trampoline act = loop0 act where
  loop0 :: JSM a -> JSM a
  loop0 before = do
    r <- before
    mcont <- popQueue
    forM_ mcont loop1
    return r
  loop1 :: JSM () -> JSM ()
  loop1 before = do
    before
    mcont <- popQueue
    forM_ mcont loop1
  popQueue :: JSM (Maybe (JSM ()))
  popQueue = state \s ->
    case Map.minViewWithKey s.ist_transaction_queue of
      Nothing -> (Nothing, s)
      Just ((_, r), newQueue) -> (Just r, s {ist_transaction_queue = newQueue})

runJSM :: InternalEnv -> JSM () -> IO ()
runJSM e c = prompt e.ien_prompt_tag $
  unJSM (trampoline c >> jsFlush) e

unsafeInsertHTML :: Text -> JSExp
unsafeInsertHTML rawHtml =
  Eval script `Apply` [PeekStack 0, Str rawHtml]
  where
    script =
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
       \})"

newScope :: JSM ScopeId
newScope = reactive newScopeFn
{-# INLINE newScope #-}

newScopeFn :: ScopeId -> InternalState -> (InternalState, ScopeId)
newScopeFn p s = (s', new) where
  new = ScopeId s.ist_id_supply
  ins = Map.insert new (Resources p [] [])
  link = Map.adjust (\r -> r {rsr_linked = new : r.rsr_linked}) p
  s' = s {
    ist_id_supply = s.ist_id_supply + 1,
    ist_resources = link $ ins s.ist_resources
  }
{-# INLINE newScopeFn #-}

localScope :: ScopeId -> JSM a -> JSM a
localScope s = local (\e -> e {ien_scope = s})
{-# INLINE localScope #-}

newRefId :: JSM RefId
newRefId = reactive newRefIdFn
{-# INLINE newRefId #-}

newRefIdFn :: ScopeId -> InternalState -> (InternalState, RefId)
newRefIdFn _e s = (s {ist_id_supply = s.ist_id_supply + 1}, RefId s.ist_id_supply)
{-# INLINE newRefIdFn #-}

freeScope :: ScopeId -> JSM ()
freeScope scope = do
  mres <- state updateState
  forM_ mres \res -> do
    forM_ (res.rsr_linked) destroyScope
    sequence_ (res.rsr_finalizers)
  jsCmd $ FreeScope scope
  where
    updateState :: InternalState -> (Maybe Resources, InternalState)
    updateState s = (res, s')
      where
        subs = Map.map (filter (not . isTargetScope)) s.ist_subscriptions
        isTargetScope sub = sub.sub_scope == scope
        (res, rsr) = Map.alterF (, Nothing) scope s.ist_resources
        s' = s {ist_subscriptions = subs, ist_resources = rsr}

destroyScope :: ScopeId -> JSM ()
destroyScope scope = do
  mres <- state updateState
  forM_ mres \res -> do
    forM_ (res.rsr_linked) destroyScope
    sequence_ (res.rsr_finalizers)
  jsCmd $ FreeScope scope
  where
    updateState :: InternalState -> (Maybe Resources, InternalState)
    updateState s = (res, s')
      where
        subs = Map.map (filter (not . isTargetScope)) s.ist_subscriptions
        isTargetScope sub = sub.sub_scope == scope
        remove = Map.alterF (,Nothing) scope
        unlink m = case res of
          Just r -> Map.adjust adj r.rsr_parent m
          Nothing -> m
        adj r = r {rsr_linked = List.filter (/=scope) r.rsr_linked}
        (res, rsr) = remove s.ist_resources
        s' = s {ist_subscriptions = subs, ist_resources = unlink rsr}

moveScope :: ScopeId -> ScopeId -> JSM ()
moveScope src dest = do
  modify updateState
  jsCmd $ MoveScope src dest
  where
    updateState :: InternalState -> InternalState
    updateState s
      | Just srcRes <- Map.lookup src s.ist_resources =
          s {ist_resources = Map.adjust (updateRes srcRes) dest s.ist_resources}
      | otherwise = s
      where
        updateRes :: Resources -> Resources -> Resources
        updateRes src' dest' = dest' {
          rsr_linked = src'.rsr_linked <> dest'.rsr_linked,
          rsr_finalizers = src'.rsr_finalizers <> dest'.rsr_finalizers
        }

installFinalizer :: JSM () -> JSM ()
installFinalizer f = reactive_ $ installFinalizerFn f
{-# INLINE installFinalizer #-}

installFinalizerFn :: JSM () -> ScopeId -> InternalState -> InternalState
installFinalizerFn k scope s = s {ist_resources = rsr}
  where
    rsr = Map.adjust ins scope s.ist_resources
    ins r = r {rsr_finalizers = k : r.rsr_finalizers}
{-# INLINE installFinalizerFn #-}

emptyState :: InternalState
emptyState = InternalState Map.empty Map.empty Map.empty 0

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

readDyn :: MonadIO m => Dynamic a -> m a
readDyn (ConstVal a) = pure a
readDyn (FromVar var) = readVar var
readDyn (MapVal val f) = fmap f $ readDyn val
readDyn (SplatVal f a) = liftA2 ($) (readDyn f) (readDyn a)
readDyn (OverrideSub _ a) = readDyn a

readVar :: MonadIO m => DynVar a -> m a
readVar (SourceVar _ ref) = liftIO $ readIORef ref
readVar (LensMap l var) = fmap (getConst . l Const) $ readVar var
readVar (OverrideVar _ var) = readVar var

newVar :: a -> JSM (DynVar a)
newVar a = do
  ref <- liftIO $ newIORef a
  let mkEv s = unsafeFromEventId $ EventId s.ist_id_supply
  state \s -> (SourceVar (mkEv s) ref, s {ist_id_supply = s.ist_id_supply + 1})

overrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
overrideVar = OverrideVar

lensMap :: Lens' s a -> DynVar s -> DynVar a
lensMap = LensMap

modifyVar :: DynVar s -> (s -> (s, a)) -> JSM a
modifyVar (SourceVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  triggerEvent varId newVal
  return a
  where
    g old = let (new, a) = f old in (new, (new, a))
modifyVar (OverrideVar ufn var) f =
  ufn (modifyVar var) f
modifyVar (LensMap l var) f = modifyVar var (swap . l (swap . f))

modifyVar_ :: DynVar s -> (s -> s) -> JSM ()
modifyVar_ var f = modifyVar var ((,()) . f)

writeVar :: DynVar s -> s -> JSM ()
writeVar var s = modifyVar_ var $ const s

forDyn :: Dynamic a -> (a -> JSM ()) -> JSM ()
forDyn dval action = readDyn dval >>= action >> subscribe dval action

forVar :: DynVar a -> (a -> JSM ()) -> JSM ()
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
modifyVarQuiet :: DynVar s -> (s -> (s, a)) -> JSM a
modifyVarQuiet (SourceVar _varId ref) f = do
  liftIO $ atomicModifyIORef' ref f
modifyVarQuiet (OverrideVar ufn var) f =
  ufn (modifyVarQuiet var) f
modifyVarQuiet (LensMap l var) f =
  modifyVarQuiet var (swap . l (swap . f))
{-# INLINEABLE modifyVarQuiet #-}

modifyVarQuiet_ :: DynVar s -> (s -> s) -> JSM ()
modifyVarQuiet_ var f = modifyVarQuiet var ((,()) . f)
{-# INLINE modifyVarQuiet_ #-}

writeVarQuiet :: DynVar s -> s -> JSM ()
writeVarQuiet var = modifyVarQuiet_ var . const
{-# INLINE writeVarQuiet #-}

jsCmd :: JSExp -> JSM ()
jsCmd cmd = JSM \e ->
  e.ien_command cmd
{-# INLINE jsCmd #-}

jsEval :: JSExp -> JSM JSVal
jsEval cmd = JSM \e -> do
  e.ien_command cmd
  e.ien_flush
{-# INLINE jsEval #-}

jsFlush :: JSM ()
jsFlush = JSM \e -> void $ e.ien_flush

commandBuffer :: CStringLen -> (CStringLen -> IO ()) -> IO (JSExp -> IO (), IO ())
commandBuffer (buf, bufSize) consume = do
  ref <- newIORef 0
  return (write ref, flush ref)
  where
    write :: IORef Int -> JSExp -> IO ()
    write ref cmd = do
      off <- readIORef ref
      let b = runBuilder $ execPut $ Binary.put cmd
      newOff <- writeCommand b off
      writeIORef ref newOff

    writeCommand :: BufferWriter -> Int -> IO Int
    writeCommand bufWrite off = do
      (written, next) <- bufWrite (buf `plusPtr` off) (bufSize - off)
      let off' = off + written
      case next of
        Done -> pure off'
        More minSize _moreWrite
          | off == 0 ->
            error $ "Buffer too small, encountered command that requires at \
                    \least " <> show minSize <> " bytes"
          | otherwise -> do
            consume (castPtr buf, off)
            writeRemains bufWrite 0
        Chunk chunk moreWrite -> do
          off1 <- writeRemains (runBuilder $ execPut $ Binary.put chunk) off
          writeRemains moreWrite off1

    writeRemains :: BufferWriter -> Int -> IO Int
    writeRemains bufWrite off = do
      (written, next) <- bufWrite (buf `plusPtr` off) (bufSize - off)
      let off' = off + written
      case next of
        Done -> pure off'
        More _minSize _moreWrite ->
          error $ "Buffer too small, inscrease the buffer size"
        Chunk chunk moreWrite -> do
          off1 <- writeRemains (runBuilder $ execPut $ Binary.put chunk) off
          writeRemains moreWrite off1

    flush :: IORef Int -> IO ()
    flush ref = do
      off <- atomicModifyIORef' ref (0,)
      consume (castPtr buf, off)

newInternalEnv :: Int -> (CStringLen -> IO ()) -> IO (InternalEnv, CStringLen)
newInternalEnv bufSize consume = do
  buf <- mallocBytes bufSize
  ien_state <- newIORef emptyState
  (write, flush) <- commandBuffer (buf, bufSize) consume
  ien_prompt_tag <- newPromptTag
  ien_continuations <- newIORef Map.empty
  let bufResult = (castPtr buf, bufSize)
  pure (
    InternalEnv {
      ien_command = write,
      ien_flush = do
        tid <- atomicModifyIORef' ien_state \s ->
          (s {ist_id_supply = s.ist_id_supply + 1}, ContId s.ist_id_supply)
        write $ Resume tid
        flush
        control ien_prompt_tag \c ->
          modifyIORef' ien_continuations $ Map.insert tid c,
      ien_state,
      ien_scope = ScopeId 0,
      ien_prompt_tag,
      ien_continuations
    }, bufResult
    )
