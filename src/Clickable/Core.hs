module Clickable.Core where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Function ((&))
import Data.IORef
import Data.List qualified as List
import Data.Text (Text)
import Data.Tuple
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import GHC.Exts hiding (build)
import Unsafe.Coerce

import Clickable.FFI
import Wasm.Compat.Prim

data DynVar a where
  DynVar :: RefId -> IORef a -> DynVar a

type RefId = Int

data DynVal a where
  TipVal :: DynVar a -> DynVal a
  TupleVal :: DynVal a -> DynVal b -> RefId -> DynVal (a, b)
  MapVal :: DynVal a -> (a -> b) -> DynVal b

-------------------------------
-- REATIVE STUFF RESURRECTED --
-------------------------------

newtype HtmlM a = HtmlM { unHtmlM :: ReaderT JSVal ClickM a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader JSVal)

newtype ClickM a = ClickM {unClickM :: ReaderT InternalEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader InternalEnv)

instance MonadState InternalState ClickM where
  state f = ClickM $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ swap . f

data InternalEnv = InternalEnv
  { scope :: ScopeId
  , internal_state_ref :: IORef InternalState
  } deriving (Generic)

data InternalState = InternalState
  { subscriptions :: [Subscription]
  , finalizers :: [Finalizer]
  , transaction_queue :: Map RefId (ClickM ())
  , next_id :: Int
  } deriving (Generic)

type Subscription = (ScopeId, RefId, Any -> ClickM ())

type Finalizer = (ScopeId, FinalizerVal)

type EventId = Int

type ScopeId = Int

data FinalizerVal
  = CustomFinalizer (ClickM ())
  | ScopeFinalizer ScopeId

emptyInternalState :: InternalState
emptyInternalState = InternalState [] [] Map.empty 0

newInternalEnv :: IO InternalEnv
newInternalEnv = do
  let scope = emptyInternalState.next_id
  internal_state_ref <- newIORef emptyInternalState
    {next_id = emptyInternalState.next_id + 1}
  return InternalEnv {internal_state_ref, scope}

liftClickM :: ClickM a -> HtmlM a
liftClickM = HtmlM . lift

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

mapVar :: DynVar a -> (a -> b) -> DynVal b
mapVar var = MapVal (TipVal var)

newVar :: a -> ClickM (DynVar a)
newVar a = do
  ref <- liftIO $ newIORef a
  state \s -> (DynVar s.next_id ref, s {next_id = s.next_id + 1})

readVal :: DynVal a -> ClickM a
readVal (TipVal (DynVar _ ref)) = liftIO $ readIORef ref
readVal (TupleVal a b _) = liftA2 (,) (readVal a) (readVal b)
readVal (MapVal val f) = fmap f $ readVal val

readVar :: DynVar a -> ClickM a
readVar (DynVar _ ref) = liftIO $ readIORef ref

atomicModifyVar :: DynVar s -> (s -> (s, a)) -> ClickM a
atomicModifyVar var@(DynVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  modify $ varModifiedFn var newVal
  return a
  where
    g old = let (new, a) = f old in (new, (new, a))

varModifiedFn ::
  DynVar a ->
  a ->
  InternalState -> InternalState
varModifiedFn (DynVar varId _) vals = go0 where
  go0 = defer varId $ gets (.subscriptions) >>= go1
  go1 [] = return ()
  go1 ((_, sVar, cb) : xs)
    | sVar == varId = cb (unsafeCoerce vals) >> go1 xs
    | otherwise = go1 xs

varModifiedFn1 :: RefId -> a -> InternalState -> InternalState
varModifiedFn1 varId vals = go0 where
  go0 = defer varId $ gets (.subscriptions) >>= go1
  go1 [] = return ()
  go1 ((_, sVar, cb) : xs)
    | sVar == varId = cb (unsafeCoerce vals) >> go1 xs
    | otherwise = go1 xs

modifyVar :: DynVar s -> (s -> s) -> ClickM ()
modifyVar var f = atomicModifyVar var ((,()) . f)

---------------------------------------------
-- RESOURCE MANAGEMENT AND DYNAMIC UPDATES --
---------------------------------------------

newScopeFn :: ScopeId -> InternalState -> (InternalState, ScopeId)
newScopeFn p s =
  let
    scopeId = s.next_id
    finalizers = (p, ScopeFinalizer scopeId) : s.finalizers
    next_id = s.next_id + 1
  in
    (s {finalizers, next_id}, scopeId)

newScope :: ClickM ScopeId
newScope = ClickM $ ReaderT $ \e ->
  atomicModifyIORef' e.internal_state_ref $ newScopeFn e.scope

freeScopeFn :: Bool -> ScopeId -> InternalState -> (InternalState, [Finalizer])
freeScopeFn unlink rscope s =
  let
    chkSub (s, _, _) = s /= rscope
    chkFin True (s1, ScopeFinalizer s2) = s1 /= rscope && s2 /= rscope
    chkFin True (s, _) = s /= rscope
    chkFin False (s, _) = s /= rscope
    (scopeFns, finalizers) = List.partition (chkFin unlink) s.finalizers
    subscriptions = List.filter chkSub s.subscriptions
  in
    (s {subscriptions, finalizers}, scopeFns)

freeScope :: Bool -> ScopeId -> ClickM ()
freeScope unlink s = f where
  f = state (swap . freeScopeFn unlink s) >>= g
  g [] = return ()
  g ((_, ScopeFinalizer s'):xs) = freeScope True s' >> g xs
  g ((_, CustomFinalizer x):xs) = x >> g xs

installFinalizerFn :: ScopeId -> ClickM () -> InternalState -> InternalState
installFinalizerFn scope k s = s {finalizers = (scope, CustomFinalizer k) : s.finalizers }

installFinalizer :: ClickM () -> ClickM ()
installFinalizer k = ask >>= \e -> modify (installFinalizerFn e.scope k)

subscribeFn ::
  ScopeId ->
  DynVal a ->
  (a -> ClickM ()) ->
  InternalState -> InternalState
subscribeFn scope (TipVal (DynVar varId _)) fn s = s
  { subscriptions = sub : s.subscriptions }
  where
    sub = (scope, varId, fn . unsafeCoerce)
subscribeFn scope (TupleVal a b varid) fn s =
  subscribeFn scope b g $ subscribeFn scope a f s
  where
    f a = do
      bval <- readVal b
      modify $ varModifiedFn1 varid (a, bval)
    g b = do
      aval <- readVal a
      modify $ varModifiedFn1 varid (aval, b)
subscribeFn scope (MapVal v f) fn s = subscribeFn scope v (fn . f) s

subscribe :: DynVal a -> (a -> ClickM ()) -> ClickM ()
subscribe val k = do
  e :: InternalEnv <- ask
  liftIO $ modifyIORef' e.internal_state_ref $ subscribeFn e.scope val k

-- | Loop until transaction queue is empty. TODO: Is this even
-- worth-while to have? What if just let multiple DOM changes when it
-- depends on several sources?
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

defer :: RefId -> ClickM () -> InternalState -> InternalState
defer k act s = s { transaction_queue = Map.insert k act s.transaction_queue }

------------------
-- BUILDING DOM --
------------------

el :: Text -> HtmlM a -> HtmlM a
el t  ch = do
  r <- ask
  el <- liftIO $ insertElement r t
  local (const el) ch

property :: Text -> Text -> HtmlM ()
property k v = do
  root <- ask
  liftIO $ setProperty root k v

boolProperty :: Text -> Bool -> HtmlM ()
boolProperty k v = do
  root <- ask
  liftIO $ setBoolProperty root k v

attribute :: Text -> Text -> HtmlM ()
attribute k v = do
  root <- ask
  liftIO $ setAttribute root k v

text :: Text -> HtmlM ()
text t = do
  r <- ask
  void $ liftIO $ insertText r t

dynText :: DynVal Text -> HtmlM ()
dynText val = do
  r <- ask
  t <- liftClickM $ readVal val
  textNode <- liftIO $ insertText r t
  liftClickM $ subscribe val $ \new ->
    liftIO $ updateTextContent textNode new

dynProp :: Text -> DynVal Text -> HtmlM ()
dynProp k val = do
  root <- ask
  v <- liftClickM $ readVal val
  liftIO $ setProperty root k v
  liftClickM $ subscribe val $ \new ->
    liftIO $ setProperty root k new

dynBoolProp :: Text -> DynVal Bool -> HtmlM ()
dynBoolProp k val = do
  root <- ask
  v <- liftClickM $ readVal val
  liftIO $ setBoolProperty root k v
  liftClickM $ subscribe val $ \new ->
    liftIO $ setBoolProperty root k new

blank :: Applicative m => m ()
blank = pure ()

---------------------
-- DYNAMIC CONTENT --
---------------------

dyn :: DynVal (HtmlM ()) -> HtmlM ()
dyn val = do
  root <- ask
  scope <- liftClickM newScope
  closeBracket <- liftIO $ js_insertBrackets root
  initialVal <- liftClickM $ readVal val
  let
    update html = do
      liftIO $ js_clearBrackets closeBracket
      html
    exec = local (\s -> s {scope}) .
      flip runReaderT closeBracket . unHtmlM
  liftClickM $ exec $ update initialVal
  liftClickM $ subscribe val \newVal -> do
    freeScope False scope
    exec $ update newVal
  return ()

--------------------------
-- START AN APPLICATION --
--------------------------

attach :: HtmlM a -> IO a
attach html = do
  body <- documentBody
  env <- newInternalEnv
  html.unHtmlM
    & flip runReaderT body
    & unClickM
    & flip runReaderT env
