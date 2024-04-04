module Clickable.Core
  ( module Clickable.Core
  , Internal.trampoline
  , Internal.syncPoint
  , Internal.launchClickM
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Function ((&))
import Data.IORef
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple

import Clickable.Internal (reactive, reactive_)
import Clickable.Internal qualified as Internal
import Clickable.Types
import Clickable.Protocol
import Clickable.Protocol.Value (Value)
import Clickable.Protocol.Value qualified as Value

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

newDynVar :: a -> ClickM (DynVar a)
newDynVar a = do
  ref <- liftIO $ newIORef a
  state \s -> (DynVar (SourceId s.next_id) ref, s {next_id = s.next_id + 1})

overrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
overrideVar = OverrideVar

readVal :: DynVal a -> ClickM a
readVal = Internal.readVal

readVar :: DynVar a -> ClickM a
readVar = Internal.readVar

modifyVar :: DynVar s -> (s -> (s, a)) -> ClickM a
modifyVar var@(DynVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  modify $ Internal.unsafeTrigger varId newVal
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

subscribe :: DynVal a -> (a -> ClickM ()) -> ClickM ()
subscribe val k = reactive_ $ Internal.subscribe val k

modifyVarQuiet :: DynVar s -> (s -> (s, a)) -> ClickM a
modifyVarQuiet var@(DynVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  return a
  where
    g s = let (s', a) = f s in (s', (s', a))
modifyVarQuiet (OverrideVar ufn var) f =
  ufn (modifyVarQuiet var) f
modifyVarQuiet (LensMap l var) f = modifyVarQuiet var (swap . l (swap . f))

modifyVarQuiet_ :: DynVar s -> (s -> s) -> ClickM ()
modifyVarQuiet_ var f = modifyVarQuiet var ((,()) . f)

--------------------------------------
-- OPERATIONS OVER evaluation_queue --
--------------------------------------

enqueueExpr :: Expr -> ClickM ()
enqueueExpr e = modify \s -> s {evaluation_queue = e : s.evaluation_queue}

evalExpr :: Expr -> ClickM Value
evalExpr e = do
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
  lift $ subscribe val $ enqueueExpr . UpdateTextNode (Var v)
  where
    f v t s = s {evaluation_queue = expr v t : s.evaluation_queue }
    expr v t = InsertNode (Arg 0 0) $ AssignVar v $ CreateTextNode t

dynProp :: Text -> DynVal Text -> HtmlM ()
dynProp k val = do
  scope <- lift $ asks (.scope)
  t <- lift $ readVal val
  v <- saveCurrentNode
  lift $ modify $ f t
  lift $ subscribe val $ enqueueExpr . ElementProp (Var v) k . String
  where
    f v s = s {evaluation_queue = expr v : s.evaluation_queue }
    expr v = ElementProp (Arg 0 0) k (String v)

dynBoolProp :: Text -> DynVal Bool -> HtmlM ()
dynBoolProp k val = do
  scope <- lift $ asks (.scope)
  t <- lift $ readVal val
  v <- saveCurrentNode
  lift $ modify $ f t
  lift $ subscribe val $ enqueueExpr . ElementProp (Var v) k . Boolean
  where
    f v s = s {evaluation_queue = expr v : s.evaluation_queue }
    expr v = ElementProp (Arg 0 0) k (Boolean v)

toggleClass :: Text -> DynVal Bool -> HtmlM ()
toggleClass className val = do
  scope <- lift $ asks (.scope)
  v <- lift $ readVal val
  n <- saveCurrentNode
  let
    initCmd False queue = queue
    initCmd True queue = InsertClassList (Arg 0 0) [className] : queue
    updateCmd False = RemoveClassList (Var n) [className]
    updateCmd True = InsertClassList (Var n) [className]
  lift $ modify \s -> s {evaluation_queue = initCmd v s.evaluation_queue}
  lift $ subscribe val $ enqueueExpr . updateCmd

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

-- | Auxilliary datatype used in 'simpleList' implementation
data InternalElem a = InternalElem
  { boundary :: VarId
  , state_var :: DynVar a
  , elem_scope :: ResourceScope
  }

-- | Display dynamic collection of widgets. NOTE: changes in `DynVar
-- a` do not automatically propagate into the larger state. See
-- `OverrideVar` and todomvc example to see one way to upstream
-- changes into the larger state.
simpleList
  :: forall a. DynVal [a]
  -- ^ Dynamic collection
  -> (Int -> DynVar a -> HtmlM ())
  -- ^ Build HTML for each element in the list
  -> HtmlM ()
simpleList listDyn h = lift do
  internalStateRef <- liftIO $ newIORef ([] :: [InternalElem a])
  boundary <- insertBoundary
  let
    exec boundary scope h = evalStateT h.unHtmlT Nothing
      & htmlBuilder1 (Var boundary)
      & local (\s -> s {scope})
    exec1 boundary = htmlBuilder1 (Var boundary)

    setup :: Int -> [a] -> [InternalElem a] -> ClickM [InternalElem a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        e <- newElem idx x
        exec e.boundary e.elem_scope $ h idx e.state_var
        fmap (e:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update existing elements along the way
      (r:rs, y:ys) -> do
        writeVar r.state_var y
        fmap (r:) $ setup (idx + 1) ys rs
    newElem :: Int -> a -> ClickM (InternalElem a)
    newElem i a = do
      elem_scope <- newScope
      local (\s -> s {scope = elem_scope}) do
        state_var <- newDynVar a
        boundary <- insertBoundary
        return InternalElem {elem_scope, state_var, boundary}
    finalizeElems :: Bool -> [InternalElem a] -> ClickM ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ destroyBoundary ee.boundary
      freeScope True ee.elem_scope
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
  initialVal <- readVal listDyn
  exec1 boundary $ updateList initialVal
  subscribe listDyn $ exec1 boundary . updateList

----------------------------
-- AUXILLIARY DEFINITIONS --
----------------------------

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
    enqueueHtml saved s = s {evaluation_queue = e : saved} where
      e = Lam (RevSeq (Arg 0 0 : s.evaluation_queue)) `Apply` [rootElm]
    saveQueue s =
      (s.evaluation_queue, s {evaluation_queue = []})

attachToBody :: HtmlM a -> ClickM a
attachToBody = attachHtml (Id "document" `Dot` "body")

blank :: Applicative m => m ()
blank = pure ()

instance (a ~ ()) => IsString (HtmlM a) where
  fromString = text . Text.pack
