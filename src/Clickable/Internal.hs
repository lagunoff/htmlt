module Clickable.Internal where

import Control.Monad.Reader
import Control.Monad.State
import Data.Binary qualified as Binary
import Data.Binary (Binary)
import Data.Foldable
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce
import Wasm.Compat.Prim

import Clickable.FFI qualified as FFI
import Clickable.Protocol
import Clickable.Types

emptyInternalState :: InternalState
emptyInternalState = InternalState [] [] Map.empty [] 0

newInternalEnv :: IO InternalEnv
newInternalEnv = mdo
  let state = emptyInternalState
  let scope = ResourceScope state.next_id
  internal_state_ref <- newIORef state {next_id = state.next_id + 1}
  receiveCb <- FFI.js_exportCallback $ receiveMessage env
  let env = InternalEnv {internal_state_ref, scope, send_message = sendMessage receiveCb}
  return env

-- | Unsafe because there is no gurantee that @a@ matches @a@ in
-- correspoding @DynVar a@ where SourceId comes from
unsafeTrigger :: SourceId -> a -> InternalState -> InternalState
unsafeTrigger sourceId vals =
  defer sourceId $ gets (.subscriptions) >>= notify
  where
    notify [] = return ()
    notify ((_, s, cb) : xs)
      | s == sourceId = cb (unsafeCoerce vals) >> notify xs
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

newVar :: ResourceScope -> InternalState -> (InternalState, VarId)
newVar e s =
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

subscribe ::
  DynVal a ->
  (a -> ClickM ()) ->
  ResourceScope ->
  InternalState -> InternalState
subscribe (ConstVal _) _ _ s = s
subscribe (FromVar (DynVar varId _)) fn scope s = s {subscriptions}
  where
    subscriptions = newSub : s.subscriptions
    newSub = (scope, varId, fn . unsafeCoerce)
subscribe (MapVal v f) fn scope s = subscribe v (fn . f) scope s
subscribe (SplatVal fv av) fn scope s =
  subscribe av g scope $ subscribe fv f scope $ attachCb s
  where
    f fv' = do
      av' <- readVal av
      modify $ unsafeTrigger varid $ fv' av'
    g av' = do
      fv' <- readVal fv
      modify $ unsafeTrigger varid $ fv' av'
    attachCb s = s
      { subscriptions = (scope, varid, fn . unsafeCoerce) : s.subscriptions
      , next_id = s.next_id + 1
      }
    varid = SourceId s.next_id
    readVal :: DynVal a -> ClickM a
    readVal (ConstVal a) = pure a
    readVal (FromVar (DynVar _ ref)) = liftIO $ readIORef ref
    readVal (MapVal val f) = fmap f $ readVal val
    readVal (SplatVal f a) = liftA2 ($) (readVal f) (readVal a)

sendMessage :: JSVal -> HaskellMessage -> IO JavaScriptMessage
sendMessage receiveCb hmsg = do
  hptr <- storeByteString $ BSL.toStrict $ Binary.encode hmsg
  jptr <- FFI.js_evalMessageFFI receiveCb hptr
  Alloc.free hptr
  Binary.decode . BSL.fromStrict <$> loadByteString jptr
  where
    storeByteString :: ByteString -> IO (Ptr a)
    storeByteString bs = do
      let len = BS.length bs
      dest <- Alloc.callocBytes (len + 8)
      poke @Word64 dest (fromIntegral len)
      BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
        copyBytes (dest `plusPtr` 8) src len
      return (castPtr dest)

    loadByteString :: Ptr a -> IO ByteString
    loadByteString ptr = do
      len <- peek @Word64 (castPtr ptr)
      let contentPtr = ptr `plusPtr` 8
      BSU.unsafePackCStringFinalizer contentPtr (fromIntegral len) (Alloc.free ptr)

receiveMessage :: InternalEnv -> Ptr Word8 -> IO ()
receiveMessage e jptr = do
  jbytes <- loadByteString jptr
  let jmsg = Binary.decode $ BSL.fromStrict jbytes
  case jmsg of
    Return _value -> return ()
    TriggerCallbackMsg arg sourceId ->
      launchClickM e $ modify (unsafeTrigger sourceId arg)
    BeforeUnload -> return ()
  where
    loadByteString :: Ptr a -> IO ByteString
    loadByteString ptr = do
      len <- peek @Word64 (castPtr ptr)
      let contentPtr = ptr `plusPtr` 8
      BSU.unsafePackCStringFinalizer contentPtr (fromIntegral len) (Alloc.free ptr)

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

loadMessage :: Binary msg => Ptr a -> IO msg
loadMessage = fmap (Binary.decode . BSL.fromStrict) . loadByteString
  where
    loadByteString :: Ptr a -> IO ByteString
    loadByteString ptr = do
      len <- peek @Word64 (castPtr ptr)
      let contentPtr = ptr `plusPtr` 8
      BSU.unsafePackCStringFinalizer contentPtr (fromIntegral len) (Alloc.free ptr)
