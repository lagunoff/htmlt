{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Data.List qualified as List
import Data.Tuple

import Clickable.Internal (reactive, reactive_)
import Clickable.Internal qualified as Internal
import Clickable.Types
import Clickable.Protocol
import Clickable.Protocol.Value

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

newVar :: a -> ClickM (DynVar a)
newVar a = do
  ref <- liftIO $ newIORef a
  let mkEv s = unsafeFromEventId $ EventId s.next_id
  state \s -> (SourceVar (mkEv s) ref, s {next_id = s.next_id + 1})

overrideVar :: (UpdateFn a -> UpdateFn a) -> DynVar a -> DynVar a
overrideVar = OverrideVar

lensMap :: Lens' s a -> DynVar s -> DynVar a
lensMap = LensMap

mapHoldVal :: (a -> b) -> DynVal a -> ClickM (DynVal b)
mapHoldVal = Internal.mapHoldVal

readVal :: MonadIO m => DynVal a -> m a
readVal = Internal.readVal

readVar :: MonadIO m => DynVar a -> m a
readVar = Internal.readVar

modifyVar :: DynVar s -> (s -> (s, a)) -> ClickM a
modifyVar var@(SourceVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  modify $ Internal.triggerEvent varId newVal
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
subscribe = Internal.subscribe

forDyn :: DynVal a -> (a -> ClickM ()) -> ClickM ()
forDyn dval action = readVal dval >>= action >> subscribe dval action

forVar :: DynVar a -> (a -> ClickM ()) -> ClickM ()
forVar = forDyn . fromVar

modifyVarQuiet :: DynVar s -> (s -> (s, a)) -> ClickM a
modifyVarQuiet var@(SourceVar varId ref) f = do
  (newVal, a) <- liftIO $ atomicModifyIORef' ref g
  return a
  where
    g s = let (s', a) = f s in (s', (s', a))
modifyVarQuiet (OverrideVar ufn var) f =
  ufn (modifyVarQuiet var) f
modifyVarQuiet (LensMap l var) f = modifyVarQuiet var (swap . l (swap . f))

modifyVarQuiet_ :: DynVar s -> (s -> s) -> ClickM ()
modifyVarQuiet_ var f = modifyVarQuiet var ((,()) . f)

-- todo: needs redesign
holdUniqDyn :: forall a. Eq a => DynVal a -> DynVal a
holdUniqDyn a = OverrideSub g a where
  g :: forall b. SubscribeFn a b -> SubscribeFn a b
  g next k = do
    old <- readVal a
    oldRef <- liftIO $ newIORef old
    next \new acc -> do
      old <- liftIO $ atomicModifyIORef' oldRef (new,)
      if (old /= new) then k new acc else return acc

--------------------------------------
-- OPERATIONS OVER evaluation_queue --
--------------------------------------

enqueueExpr :: Expr -> ClickM ()
enqueueExpr e = modify \s -> s {evaluation_queue = e : s.evaluation_queue}

evalExpr :: Expr -> ClickM Value
evalExpr e = do
  eval_expr <- asks (.eval_expr)
  queue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  liftIO $ eval_expr $ RevSeq $ e : queue

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
    applyFin (ScopeFinalizer{sf_linked_scope}:xs) = freeScope True sf_linked_scope >> applyFin xs
    applyFin (CustomFinalizer{cf_callback}:xs) = cf_callback >> applyFin xs

installFinalizer :: ClickM () -> ClickM ()
installFinalizer k = reactive_ $ Internal.installFinalizer k

newVarId :: ClickM VarId
newVarId = reactive Internal.newVarId

newCallback :: (a -> ClickM ()) -> ClickM (Event a)
newCallback k = reactive $ Internal.newCallback k

------------------
-- BUILDING DOM --
------------------

el :: Text -> HtmlM a -> HtmlM a
el t = lift . customizeHtml True (DomBuilder (CreateElement t))

elns :: Text -> Text -> HtmlM a -> HtmlM a
elns ns t = lift . customizeHtml True (DomBuilder (CreateElementNS ns t))

property :: ToValue a => Text -> a -> HtmlM ()
property k v = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = ElementProp k (toExpr v)

attribute :: Text -> Text -> HtmlM ()
attribute k v = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = ElementAttr k v

text :: Text -> HtmlM ()
text t = lift $ modify f where
  f s = s {evaluation_queue = expr : s.evaluation_queue }
  expr = InsertNode $ CreateTextNode t

dynText :: DynVal Text -> HtmlM ()
dynText val = do
  t <- lift $ readVal val
  v <- lift $ newVarId
  lift $ modify $ f v t
  lift $ subscribe val $ enqueueExpr . UpdateTextNode (Var v)
  where
    f v t s = s {evaluation_queue = expr v t : s.evaluation_queue }
    expr v t = InsertNode $ AssignVar v $ CreateTextNode t

dynProp :: ToValue a => Text -> DynVal a -> HtmlM ()
dynProp k val = do
  t <- lift $ readVal val
  v <- saveDomBuilder
  lift $ modify $ f t
  lift $ subscribe val $ enqueueExpr . SupplyDomBuilder (DomBuilder (Var v)) . ElementProp k . toExpr
  where
    f v s = s {evaluation_queue = expr v : s.evaluation_queue }
    expr v = ElementProp k (toExpr v)

dynAttr :: Text -> DynVal Text -> HtmlM ()
dynAttr k val = do
  t <- lift $ readVal val
  v <- saveDomBuilder
  lift $ modify $ f t
  lift $ subscribe val $ enqueueExpr . SupplyDomBuilder (DomBuilder (Var v)) . ElementAttr k
  where
    f v s = s {evaluation_queue = expr v : s.evaluation_queue }
    expr v = ElementAttr k v

toggleClass :: Text -> DynVal Bool -> HtmlM ()
toggleClass className val = do
  scope <- lift $ asks (.scope)
  v <- lift $ readVal val
  n <- saveDomBuilder
  let
    initCmd False queue = queue
    initCmd True queue = ClassListAdd [className] : queue
    updateCmd False = SupplyDomBuilder (DomBuilder (Var n)) $ ClassListRemove [className]
    updateCmd True = SupplyDomBuilder (DomBuilder (Var n)) $ ClassListAdd [className]
  lift $ modify \s -> s {evaluation_queue = initCmd v s.evaluation_queue}
  lift $ subscribe val $ enqueueExpr . updateCmd

dynClassList :: DynVal [Text] -> HtmlM ()
dynClassList dynList = do
  n <- saveDomBuilder
  scope <- lift $ asks (.scope)
  initVal <- lift $ readVal dynList
  let
    compareList as bs =
      (diffList as bs, diffList bs as)
    diffList as bs = List.foldl'
      (\xs k -> if List.elem k as then xs else k:xs) [] bs
    k newList (_, _, oldList) = do
      let (added, removed) = compareList oldList newList
      modQueue $ updateCmd (added, removed, newList)
      return (added, removed, newList)
    updateCmd ([], [], _) queue = queue
    updateCmd (added, [], _) queue =
      SupplyDomBuilder (DomBuilder (Var n)) (ClassListAdd added) : queue
    updateCmd ([], removed, _) queue =
      SupplyDomBuilder (DomBuilder (Var n)) (ClassListRemove removed) : queue
    updateCmd (added, removed, _) queue
      = SupplyDomBuilder (DomBuilder (Var n)) (ClassListRemove removed)
      : SupplyDomBuilder (DomBuilder (Var n)) (ClassListAdd added)
      : queue
    modQueue f = modify \s -> s {evaluation_queue = f s.evaluation_queue}
  lift $ enqueueExpr $ ClassListAdd initVal
  lift $ Internal.subscribeAccum dynList k ([], [], initVal)

---------------------
-- DYNAMIC CONTENT --
---------------------

dyn :: DynVal (HtmlM ()) -> HtmlM ()
dyn val = do
  brackets <- lift insertBrackets
  scope <- lift newScope
  initialVal <- lift $ readVal val
  let
    update html = do
      lift $ clearBrackets brackets
      html
    exec h =
      customizeHtml False (DomBuilder (Var brackets)) h
      & local (\s -> s {scope})
  lift $ exec $ update initialVal
  lift $ subscribe val $ \newVal -> do
    freeScope False scope
    exec $ update newVal

-- | Auxilliary datatype used in 'simpleList' implementation
data ElemEnv a = ElemEnv
  { brackets :: VarId
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
  internalStateRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  brackets <- insertBrackets
  let
    exec brackets scope h =
      customizeHtml False (DomBuilder (Var brackets)) h
        & local (\s -> s {scope})
    exec1 brackets = customizeHtml False (DomBuilder (Var brackets))

    setup :: Int -> [a] -> [ElemEnv a] -> ClickM [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        e <- newElem idx x
        exec e.brackets e.elem_scope $ h idx e.state_var
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
    newElem :: Int -> a -> ClickM (ElemEnv a)
    newElem i a = do
      elem_scope <- newScope
      local (\s -> s {scope = elem_scope}) do
        state_var <- newVar a
        brackets <- insertBrackets
        return ElemEnv {elem_scope, state_var, brackets}
    finalizeElems :: Bool -> [ElemEnv a] -> ClickM ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ detachBrackets ee.brackets
      freeScope True ee.elem_scope
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
  initialVal <- readVal listDyn
  exec1 brackets $ lift $ updateList initialVal
  subscribe listDyn $ exec1 brackets . lift . updateList

-- | Parse given text as HTML and attach the resulting tree to
-- 'html_current_element'. This way you can create not only HTML but
-- anything that @innerHTML@ property can create (e.g. SVG)
--
-- > -- Create a div with an SVG image inside that shows a black
-- > -- circle
-- > div_ [] do
-- >   unsafeHtml "<svg viewBox="0 0 100 100">\
-- >     \<circle cx="50" cy="50" r="50"/>\
-- >     \</svg>"
unsafeHtml :: Text -> HtmlM ()
unsafeHtml rawHtml = lift $ enqueueExpr (Internal.unsafeInsertHtml rawHtml)

-- | Attach resulting DOM to the given node instead of where it
-- supposed to go given the cotext it used in. Might be useful for
-- implementing modal dialogs, tooltips etc. Similar to what called
-- portals in React ecosystem
portal :: DomBuilder -> HtmlM a -> HtmlM a
portal newRootEl html =
  lift $ customizeHtml False newRootEl do
    brackets <- lift insertBrackets
    result <- lift $ customizeHtml False (DomBuilder (Var brackets)) html
    lift $ installFinalizer $ detachBrackets brackets
    return result

blank :: Applicative m => m ()
blank = pure ()

----------------------------
-- AUXILLIARY DEFINITIONS --
----------------------------

customizeHtml :: Bool -> DomBuilder -> HtmlM a -> ClickM a
customizeHtml doInsert builder content = do
  prevQueue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  result <- evalStateT content.unHtmlT Nothing
  modify \s -> s {evaluation_queue = mkExpr s.evaluation_queue : prevQueue}
  return result
  where
    mkExpr seq
      | doInsert = InsertNode $ SupplyDomBuilder builder $ RevSeq (AskDomBuilder : seq)
      | otherwise = SupplyDomBuilder builder $ RevSeq (AskDomBuilder : seq)

attachHtml :: DomBuilder -> HtmlM a -> ClickM a
attachHtml rootEl contents = do
  savedQueue <- state saveQueue
  evalStateT contents.unHtmlT Nothing <*
    modify (enqueueHtml savedQueue)
  where
    enqueueHtml saved s = s {evaluation_queue = e : saved} where
      e = SupplyDomBuilder rootEl (RevSeq s.evaluation_queue)
    saveQueue s =
      (s.evaluation_queue, s {evaluation_queue = []})

attachToBody :: HtmlM a -> ClickM a
attachToBody = attachHtml $ DomBuilder $ Id "document" `Dot` "body"

saveDomBuilder :: HtmlM VarId
saveDomBuilder = do
  alreadySaved <- HtmlT get
  case alreadySaved of
    Nothing -> do
      varId <- lift newVarId
      HtmlT $ put $ Just varId
      lift $ enqueueExpr $ AssignVar varId AskDomBuilder
      return varId
    Just saved -> return saved

insertBrackets :: ClickM VarId
insertBrackets = do
  brackets <- newVarId
  enqueueExpr $ AssignVar brackets InsertBrackets
  return brackets

clearBrackets :: VarId -> ClickM ()
clearBrackets n =
  enqueueExpr $ SupplyDomBuilder (DomBuilder (Var n)) $ ClearBrackets False

detachBrackets :: VarId -> ClickM ()
detachBrackets n =
  enqueueExpr $ SupplyDomBuilder (DomBuilder (Var n)) $ ClearBrackets True

instance (a ~ ()) => IsString (HtmlM a) where
  fromString = text . Text.pack
