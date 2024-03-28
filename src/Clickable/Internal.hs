module Clickable.Internal where

import Clickable.Types
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List qualified as List
import Data.Map qualified as Map
import Unsafe.Coerce


emptyInternalState :: InternalState
emptyInternalState = InternalState [] [] Map.empty 0

newInternalEnv :: IO InternalEnv
newInternalEnv = do
  let scope = ResourceScope emptyInternalState.next_id
  internal_state_ref <- newIORef emptyInternalState
    {next_id = emptyInternalState.next_id + 1}
  return InternalEnv {internal_state_ref, scope}

unsafeTrigger :: SourceId -> a -> InternalState -> InternalState
unsafeTrigger varId vals = go0 where
  go0 = defer varId $ gets (.subscriptions) >>= go1
  go1 [] = return ()
  go1 ((_, sVar, cb) : xs)
    | sVar == varId = cb (unsafeCoerce vals) >> go1 xs
    | otherwise = go1 xs

newScope :: ResourceScope -> InternalState -> (InternalState, ResourceScope)
newScope p s =
  let
    scopeId = ResourceScope s.next_id
    finalizers = (p, ScopeFinalizer scopeId) : s.finalizers
    next_id = s.next_id + 1
  in
    (s {finalizers, next_id}, scopeId)

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

defer :: SourceId -> ClickM () -> InternalState -> InternalState
defer k act s = s { transaction_queue = Map.insert k act s.transaction_queue }

reactive :: (ResourceScope -> InternalState -> (InternalState, a)) -> ClickM a
reactive f = ClickM $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ f e.scope

reactive_ :: (ResourceScope -> InternalState -> InternalState) -> ClickM ()
reactive_ f = reactive \scope s -> (f scope s, ())
