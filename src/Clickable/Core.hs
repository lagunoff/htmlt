module Clickable.Core
  ( module Clickable.Core
  , Internal.trampoline
  , Internal.syncPoint
  , Internal.launchClickM
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Function ((&))
import Data.Text (Text)

import Clickable.Internal (reactive, reactive_)
import Clickable.Internal qualified as Internal
import Clickable.Types
import Clickable.Protocol
import Clickable.Protocol.Value (Value)
import Clickable.Protocol.Value qualified as Value

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

mapVar :: DynVar a -> (a -> b) -> DynVal b
mapVar var = MapVal (FromVar var)

newDynVar :: a -> ClickM (DynVar a)
newDynVar a = do
  ref <- liftIO $ newIORef a
  state \s -> (DynVar (SourceId s.next_id) ref, s {next_id = s.next_id + 1})

readVal :: DynVal a -> ClickM a
readVal (ConstVal a) = pure a
readVal (FromVar (DynVar _ ref)) = liftIO $ readIORef ref
readVal (MapVal val f) = fmap f $ readVal val
readVal (SplatVal f a) = liftA2 ($) (readVal f) (readVal a)

readVar :: DynVar a -> ClickM a
readVar (DynVar _ ref) = liftIO $ readIORef ref

atomicModifyVar :: DynVar s -> (s -> (s, a)) -> ClickM a
atomicModifyVar var@(DynVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  modify $ Internal.unsafeTrigger varId newVal
  return a
  where
    g old = let (new, a) = f old in (new, (new, a))

modifyVar :: DynVar s -> (s -> s) -> ClickM ()
modifyVar var f = atomicModifyVar var ((,()) . f)

writeVar :: DynVar s -> s -> ClickM ()
writeVar var s = modifyVar var $ const s

subscribe :: DynVal a -> (a -> ClickM ()) -> ClickM ()
subscribe val k = reactive_ $ Internal.subscribe val k

--------------------------------------
-- OPERATIONS OVER evaluation_queue --
--------------------------------------

enqueueExpr :: Expr -> ClickM ()
enqueueExpr e = modify \s -> s {evaluation_queue = e : s.evaluation_queue}

evalSync :: Expr -> ClickM Value
evalSync e = do
  send_message <- asks (.send_message)
  queue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  result <- liftIO $ send_message $ EvalExpr $ RevSeq $ e : queue
  case result of
    Return v -> return v
    _ -> return Value.Null

-------------------------
-- RESOURCE MANAGEMENT --
-------------------------

newScope :: ClickM ResourceScope
newScope = reactive Internal.newScope

freeScope :: Bool -> ResourceScope -> ClickM ()
freeScope unlink s =
  reactive (const (Internal.freeScope unlink s)) >>= applyFin
  where
    applyFin [] = enqueueExpr $ FreeScope s
    applyFin ((_, ScopeFinalizer s'):xs) = freeScope True s' >> applyFin xs
    applyFin ((_, CustomFinalizer x):xs) = x >> applyFin xs

installFinalizer :: ClickM () -> ClickM ()
installFinalizer k = reactive_ $ Internal.installFinalizer k

newVar :: ClickM VarId
newVar = reactive Internal.newVar

------------------
-- BUILDING DOM --
------------------

el :: Text -> HtmlM a -> HtmlM a
el t ch = lift $ htmlBuilder (CreateElement t) $ evalStateT ch.unHtmlT Nothing

property :: Text -> Text -> HtmlM ()
property k v = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = ElementProp (Arg 0 0) k (String v)

boolProperty :: Text -> Bool -> HtmlM ()
boolProperty k v = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = ElementProp (Arg 0 0) k (Boolean v)

attribute :: Text -> Text -> HtmlM ()
attribute k v = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = ElementAttr (Arg 0 0) k v

text :: Text -> HtmlM ()
text t = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = InsertNode (Arg 0 0) $ CreateTextNode t

dynText :: DynVal Text -> HtmlM ()
dynText val = do
  scope <- lift $ asks (.scope)
  t <- lift $ readVal val
  v <- lift $ newVar
  lift $ modify $ f v t
  lift $ subscribe val $ enqueueIfAlive scope . UpdateTextNode (Var v)
  where
    f v t s = s {evaluation_queue = expr v t : s.evaluation_queue }
    expr v t = InsertNode (Arg 0 0) $ AssignVar v $ CreateTextNode t

dynProp :: Text -> DynVal Text -> HtmlM ()
dynProp k val = do
  scope <- lift $ asks (.scope)
  t <- lift $ readVal val
  v <- saveCurrentNode
  lift $ modify $ f t
  lift $ subscribe val $ enqueueIfAlive scope . ElementProp (Var v) k . String
  where
    f v s = s {evaluation_queue = expr v : s.evaluation_queue }
    expr v = ElementProp (Arg 0 0) k (String v)

dynBoolProp :: Text -> DynVal Bool -> HtmlM ()
dynBoolProp k val = do
  scope <- lift $ asks (.scope)
  t <- lift $ readVal val
  v <- saveCurrentNode
  lift $ modify $ f t
  lift $ subscribe val $ enqueueIfAlive scope . ElementProp (Var v) k . Boolean
  where
    f v s = s {evaluation_queue = expr v : s.evaluation_queue }
    expr v = ElementProp (Arg 0 0) k (Boolean v)

blank :: Applicative m => m ()
blank = pure ()

---------------------
-- DYNAMIC CONTENT --
---------------------

dyn :: DynVal (HtmlM ()) -> HtmlM ()
dyn val = do
  boundary <- lift insertBoundary
  scope <- lift newScope
  initialVal <- lift $ readVal val
  let
    update html = do
      lift $ clearBoundary boundary
      html
    exec h = evalStateT h.unHtmlT Nothing
      & htmlBuilder1 (Var boundary)
      & local (\s -> s {scope})
  lift $ exec $ update initialVal
  lift $ subscribe val $ \newVal -> do
    freeScope False scope
    exec $ update newVal

htmlBuilder :: Expr -> ClickM a -> ClickM a
htmlBuilder builder content = do
  prevQueue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  result <- content
  let mkExpr revSeq = InsertNode (Arg 0 0) (Lam (RevSeq (Arg 0 0 : revSeq)) `Apply` [builder])
  modify \s -> s {evaluation_queue = mkExpr s.evaluation_queue : prevQueue}
  return result

htmlBuilder1 :: Expr -> ClickM a -> ClickM a
htmlBuilder1 builder content = do
  prevQueue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  result <- content
  let mkExpr revSeq = Apply (Lam (RevSeq (Arg 0 0 : revSeq))) [builder]
  modify \s -> s {evaluation_queue = mkExpr s.evaluation_queue : prevQueue}
  return result

enqueueIfAlive :: ResourceScope -> Expr -> ClickM ()
enqueueIfAlive scope e = modify \s ->
  let
    evaluation_queue =
      -- if Map.member scope s.finalizers
      if True
        then e : s.evaluation_queue else s.evaluation_queue
  in
    s {evaluation_queue}

saveCurrentNode :: HtmlM VarId
saveCurrentNode = do
  alreadySaved <- HtmlT get
  case alreadySaved of
    Nothing -> do
      varId <- lift newVar
      HtmlT $ put $ Just varId
      lift $ enqueueExpr $ AssignVar varId (Arg 0 0)
      return varId
    Just saved -> return saved

insertBoundary :: ClickM VarId
insertBoundary = do
  boundary <- newVar
  enqueueExpr $ AssignVar boundary (InsertBoundary (Arg 0 0))
  return boundary

clearBoundary :: VarId -> ClickM ()
clearBoundary boundary = enqueueExpr (ClearBoundary (Var boundary) False)

destroyBoundary :: VarId -> ClickM ()
destroyBoundary boundary = enqueueExpr (ClearBoundary (Var boundary) True)

attachHtml :: Expr -> HtmlM a -> ClickM a
attachHtml rootElm contents = do
  savedQueue <- state saveQueue
  evalStateT contents.unHtmlT Nothing
    <* modify (enqueueHtml savedQueue)
  where
    enqueueHtml savedQueue s = s {evaluation_queue = e : savedQueue}
      where
        e = Lam (RevSeq (Arg 0 0 : s.evaluation_queue)) `Apply` [rootElm]
    saveQueue s =
      (s.evaluation_queue, s {evaluation_queue = []})

attachToBody :: HtmlM a -> ClickM a
attachToBody = attachHtml (Id "document" `Dot` "body")
