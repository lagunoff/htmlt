module Clickable where

import Control.Monad
import Data.Bifunctor
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.State
import Data.Foldable
import Data.Function ((&))
import Data.Generics.Internal.VL hiding (build)
import Data.Generics.Product.Fields
import Data.Generics.Sum.Constructors
import Data.IORef
import Data.List qualified as List
import Data.Text (Text)
import Data.Tuple
import Data.Typeable
import Data.Map (Map)
import Data.Set (Set)
import Data.Map qualified as Map
import GHC.Generics
import GHC.Exts hiding (build)
import GHC.TypeLits
import Unsafe.Coerce

import Clickable.FFI
import Wasm.Compat.Prim

---------------------------
-- INCREMENTAL VARIABLES --
---------------------------

data IncVar a where
  IncVar :: RefId -> IORef a -> IncVar a

type RefId = Int

data IncVal a where
  TipVal :: IncVar a -> IncVal a
  MapVal :: IncVal a -> (a -> b) -> IncVal b

newVar :: a -> ClickM (IncVar a)
newVar a = do
  ref <- liftIO $ newIORef a
  state \s -> (IncVar s.next_id ref, s {next_id = s.next_id + 1})

readVal :: IncVal a -> IO a
readVal (TipVal (IncVar _ ref)) = readIORef ref
readVal (MapVal val f) = fmap f $ readVal val

readVar :: IncVar a -> IO a
readVar (IncVar _ ref) = readIORef ref

atomicModifyVar :: IncVar s -> (s -> (s, a)) -> ClickM a
atomicModifyVar var@(IncVar varId ref) f = do
  (vals, a) <- liftIO $ atomicModifyIORef' ref g
  modify $ varModifiedFn var vals
  return a
  where
    g old = let (new, a) = f old in (new, ((old, new), a))

modifyVar :: IncVar s -> (s -> s) -> ClickM ()
modifyVar var f = atomicModifyVar var ((,()) . f)

subscribeFn ::
  ScopeId ->
  IncVal a ->
  ((a, a) -> ClickM ()) ->
  InternalState -> InternalState
subscribeFn s (TipVal (IncVar varId _)) fn old =
  old { subscriptions = sub : old.subscriptions }
  where
    sub = (s, varId, fn . bimap unsafeCoerce unsafeCoerce)
subscribeFn s (MapVal v f) fn old = subscribeFn s v (fn . bimap f f) old

subscribe :: IncVal a -> ((a, a) -> ClickM ()) -> ClickM ()
subscribe val k = do
  e <- ask
  liftIO $ modifyIORef' e.internal_state_ref $ subscribeFn e.scope val k

varModifiedFn ::
  IncVar a ->
  (a, a) ->
  InternalState -> InternalState
varModifiedFn (IncVar varId _) vals = go0 where
  go0 = defer varId $ gets (.subscriptions) >>= go1
  go1 [] = return ()
  go1 ((_, sVar, cb) : xs)
    | sVar == varId = cb (bimap unsafeCoerce unsafeCoerce vals) >> go1 xs
    | otherwise = go1 xs

freeScopeFn :: Bool -> ScopeId -> InternalState -> (InternalState, [Finalizer])
freeScopeFn unlink rscope old =
  let
    chkSub (s, _, _) = s /= rscope
    chkFin True (s1, ScopeFinalizer s2) = s1 /= rscope && s2 /= rscope
    chkFin True (s, _) = s /= rscope
    chkFin False (s, _) = s /= rscope
    (scopeFns, finalizers) = List.partition (chkFin unlink) old.finalizers
    new = old
      { subscriptions = List.filter chkSub old.subscriptions
      , finalizers
      }
  in
    (new, scopeFns)

freeScope :: ScopeId -> ClickM ()
freeScope s = f where
  f = state (swap . freeScopeFn True s) >>= g
  g [] = return ()
  g ((_, ScopeFinalizer s'):xs) = freeScope s' >> g xs
  g ((_, CustomFinalizer x):xs) = x >> g xs

clearScope :: ScopeId -> ClickM ()
clearScope s = f where
  f = state (swap . freeScopeFn False s) >>= g
  g [] = return ()
  g ((_, ScopeFinalizer s'):xs) = freeScope s' >> g xs
  g ((_, CustomFinalizer x):xs) = x >> g xs

newScopeFn :: ScopeId -> InternalState -> (InternalState, ScopeId)
newScopeFn p old =
  let
    scopeId = old.next_id
    new = old
      { finalizers = (p, ScopeFinalizer scopeId) : old.finalizers
      , next_id = old.next_id + 1
      }
  in
    (new, scopeId)

newScope :: ClickM ScopeId
newScope = ClickM $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ newScopeFn e.scope

-------------------------------
-- REATIVE STUFF RESURRECTED --
-------------------------------

data InternalState = InternalState
  { subscriptions :: [Subscription]
  , finalizers :: [Finalizer]
  , transaction_queue :: Map RefId (ClickM ())
  , next_id :: Int
  } deriving (Generic)

emptyInternalState :: InternalState
emptyInternalState = InternalState
  { subscriptions = []
  , finalizers = []
  , transaction_queue = Map.empty
  , next_id = 0
  }

data InternalEnv = InternalEnv
  { scope :: ScopeId
  , internal_state_ref :: IORef InternalState
  } deriving (Generic)

newInternalEnv :: IO InternalEnv
newInternalEnv = do
  let scope = emptyInternalState.next_id
  internal_state_ref <- newIORef emptyInternalState
  return InternalEnv {internal_state_ref, scope}

type Subscription = (ScopeId, RefId, (Any, Any) -> ClickM ())

type Finalizer = (ScopeId, FinalizerVal)

type EventId = Int

type ScopeId = Int

data FinalizerVal
  = CustomFinalizer (ClickM ())
  | ScopeFinalizer ScopeId

newtype HtmlM a = HtmlM { unHtmlM :: ReaderT JSVal ClickM a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader JSVal)

newtype ClickM a = ClickM {unClickM :: ReaderT InternalEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader InternalEnv)

instance MonadState InternalState ClickM where
  state f = ClickM $ ReaderT $ \e -> atomicModifyIORef' e.internal_state_ref $ swap . f

liftClickM :: ClickM a -> HtmlM a
liftClickM = HtmlM . lift

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

el :: Text -> [(Text, Text)] -> HtmlM a -> HtmlM a
el t att ch = do
  r <- ask
  el <- liftIO $ insertElement r t
  liftIO $ forM_ att \(k, v) -> setProperty el k v
  local (const el) ch

text :: Text -> HtmlM ()
text t = do
  r <- ask
  void $ liftIO $ insertText r t

dynText :: IncVal Text -> HtmlM ()
dynText val = do
  r <- ask
  s <- HtmlM $ lift $ asks (.scope)
  t <- liftIO $ readVal val
  textNode <- liftIO $ insertText r t
  HtmlM $ lift $ modify $ subscribeFn s val $ \(old, new) -> liftIO $
    when (old /= new) $ updateTextContent textNode new

div_ :: [(Text, Text)] -> HtmlM a -> HtmlM a
div_ = el "div"

canvas_ :: [(Text, Text)] -> HtmlM a -> HtmlM a
canvas_ = el "div"

span_ :: [(Text, Text)] -> HtmlM a -> HtmlM a
span_ = el "div"

h1_ :: [(Text, Text)] -> HtmlM a -> HtmlM a
h1_ = el "h1"

button_ :: [(Text, Text)] -> HtmlM a -> HtmlM a
button_ = el "button"

select_ :: [(Text, Text)] -> HtmlM a -> HtmlM a
select_ = el "select"

on :: Text -> (JSVal -> ClickM ()) -> HtmlM ()
on e l = do
  root <- ask
  env <- HtmlM $ lift ask
  let cb = void . (`runReaderT` env) . unClickM . trampoline . l
  jlisnr <- liftIO $ js_dynExport cb
  liftIO $ addEventListener root e jlisnr

---------------------
-- DYNAMIC CONTENT --
---------------------

dyn :: IncVal (HtmlM ()) -> HtmlM ()
dyn val = do
  root <- ask
  closeBracket <- liftIO $ js_insertBrackets root
  scope <- liftClickM newScope
  let
    update html = do
      liftIO $ js_clearBrackets closeBracket
      html
    exec = local (\s -> s {scope}) .
      flip runReaderT closeBracket . unHtmlM
  liftClickM $ exec $ update undefined
  liftClickM $ subscribe val \(_, newVal) -> do
    clearScope scope
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
