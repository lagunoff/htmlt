module Clickable.Core where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map qualified as Map
import Data.Text (Text)

import Clickable.FFI
import Clickable.Internal (reactive, reactive_)
import Clickable.Internal qualified as Internal
import Clickable.Types
import Wasm.Compat.Prim


launchHtmlM :: JSVal -> InternalEnv -> HtmlM a -> IO a
launchHtmlM root env =
  flip runReaderT env . unClickM . trampoline . flip runReaderT root . unHtmlM

launchClickM :: InternalEnv -> ClickM a -> IO a
launchClickM env = flip runReaderT env . unClickM . trampoline

liftClickM :: ClickM a -> HtmlM a
liftClickM = HtmlM . lift

---------------------------------------
-- OPERATIONS OVER DYNAMIC VARIABLES --
---------------------------------------

mapVar :: DynVar a -> (a -> b) -> DynVal b
mapVar var = MapVal (FromVar var)

newVar :: a -> ClickM (DynVar a)
newVar a = do
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

-------------------------
-- RESOURCE MANAGEMENT --
-------------------------

newScope :: ClickM ResourceScope
newScope = reactive Internal.newScope

freeScope :: Bool -> ResourceScope -> ClickM ()
freeScope unlink s = f where
  f = reactive (const (Internal.freeScope unlink s)) >>= g
  g [] = return ()
  g ((_, ScopeFinalizer s'):xs) = freeScope True s' >> g xs
  g ((_, CustomFinalizer x):xs) = x >> g xs

installFinalizer :: ClickM () -> ClickM ()
installFinalizer k = reactive_ $ Internal.installFinalizer k

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
