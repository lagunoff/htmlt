 module HtmlT.Sketch where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable
import Data.Typeable
import Data.Tuple
import Data.IORef
import Control.Monad
import GHC.TypeLits
import Data.Text (Text)
import Data.List qualified as List
import Data.Proxy
import Data.Generics.Product.Fields
import Data.Generics.Internal.VL hiding (build)
import Data.Generics.Sum.Constructors

import HtmlT.Sketch.FFI
import Wasm.Compat.Prim

--------------------------
-- DIFFING AND PATCHING --
--------------------------

data Edit a where
  Ins :: a -> Edit a
  Fld :: (HasField' f s a, KnownSymbol f, Typeable a) => Proxy f -> Edit a -> Edit s
  Ctor :: AsConstructor' ctor s a => Proxy ctor -> Edit a -> Edit s
  Ix :: Int -> Edit e -> Edit [e]
  Splice :: Int -> Int -> [e] -> Edit [e]

data SyncOp s where
  TextOp :: JSVal -> (s -> Text) -> SyncOp s
  PropOp :: JSVal -> Text -> (s -> Text) -> SyncOp s
  MapOp :: (Jet s -> Jet t) -> SyncOp t -> SyncOp s

data Jet a = Jet { position :: a, velocity :: [Edit a] }

type SyncPlan s = [SyncOp s]

emptySyncPlan :: SyncPlan s
emptySyncPlan = []

data Layout s where
  Embed :: (ModifierFn s -> ModifierFn t) -> (Jet s -> Jet t) -> [Layout t] -> Layout s
  TextNode :: Text -> Layout s
  DynText :: (s -> Text) -> Layout s
  ElemNode :: Text -> [(Text, Text)] -> [Layout s] -> Layout s
  EventListener :: Text -> (JSVal -> TransitionM s ()) -> Layout s
  -- DynNode :: (Jet s -> Jet (Layout s)) -> Layout s
  -- SimpleList :: (Jet s -> Jet [a]) -> (Edit [a] -> Edit s) -> Layout a -> Layout s

patch :: Edit s -> s -> s
patch e s = case e of
  Ins s -> s
  Fld p e -> pfield p e s
    where
      pfield :: forall f s a. HasField' f s a => Proxy f -> Edit a -> s -> s
      pfield _ e s = over (field' @f) (patch e) s
  Ctor p e -> pctor p e s
    where
      pctor :: forall ctor s a. AsConstructor' ctor s a => Proxy ctor -> Edit a -> s -> s
      pctor _ e s = over (_Ctor' @ctor) (patch e) s
  Ix i e -> go i e s
    where
      go :: Int -> Edit e -> [e] -> [e]
      go 0 e (x:xs) = patch e x : xs
      go _ _ [] = []
      go n e (x:xs) = x : go (n - 1) e xs
  Splice ix remove insert -> go insert ix s
    where
      go :: [e] -> Int -> [e] -> [e]
      go ins 0 xs = ins <> List.drop remove xs
      go ins n (x:xs) = x : go ins (n - 1) xs
      go _ _ [] = []

-----------------
-- CORE TYPES  --
-----------------

newtype BuilderM s a = BuilderM { unBuilderM :: StateT [Layout s] Identity a }
  deriving newtype (Functor, Applicative, Monad, MonadState [Layout s])

newtype TransitionM s a = TransitionM { unTransitionM :: ReaderT (ModifierFn s) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadState (Jet s) (TransitionM s) where
  state f = TransitionM $ ReaderT \(ModifierFn mf) -> mf (swap . f)

newtype ModifierFn s = ModifierFn {unModifierFn :: forall a. (Jet s -> (Jet s, a)) -> IO a }

----------------------
-- STATE MANAGEMENT --
----------------------

edit :: Edit s -> TransitionM s ()
edit e = modify \j -> Jet (patch e j.position) (e : j.velocity)

modifyFld :: forall f s a m.
  ( HasField' f s a
  , KnownSymbol f
  , Typeable a
  , MonadState (Jet s) m
  ) =>
  (a -> a) ->
  m ()
modifyFld f = modify f0 where
  f0 :: Jet s -> Jet s
  f0 j = Jet s' (e : j.velocity) where
    s' = over (field' @f) f j.position
    e = Fld (Proxy @f) (Ins (getField @f s'))

mapModifierFn :: forall s a.
  (Edit a -> Edit s) ->
  (Jet s -> Jet a) ->
  ModifierFn s ->
  ModifierFn a
mapModifierFn g f (ModifierFn ms) = ModifierFn f0 where
  f0 :: forall x. (Jet a -> (Jet a, x)) -> IO x
  f0 fa = ms (f1 fa)

  f1 :: forall x. (Jet a -> (Jet a, x)) -> Jet s -> (Jet s, x)
  f1 fa js = (js', x) where
    (ja, x) = fa (f js)
    js' = Jet js.position (fmap g ja.velocity <> js.velocity)

refModifier :: forall s. (Jet s -> IO ()) -> IORef s -> ModifierFn s
refModifier up ref = ModifierFn m0 where
  m0 :: forall x. (Jet s -> (Jet s, x)) -> IO x
  m0 f = do
    (x, jet) <- atomicModifyIORef' ref $ m1 f
    up jet
    return x

  m1 :: forall x. (Jet s -> (Jet s, x)) -> s -> (s, (x, Jet s))
  m1 f s = let (js', x) = f (Jet s []) in (js'.position, (x, js'))

fldModifierFn :: forall f s a.
  ( HasField' f s a
  , KnownSymbol f
  , Typeable a
  ) =>
  ModifierFn s ->
  ModifierFn a
fldModifierFn (ModifierFn ms) = ModifierFn f0 where
  f0 :: forall x. (Jet a -> (Jet a, x)) -> IO x
  f0 fa = ms (f1 fa)

  f1 :: forall x. (Jet a -> (Jet a, x)) -> Jet s -> (Jet s, x)
  f1 fa js = (js', x) where
    a = getField @f js.position
    s' = setField @f ja.position js.position
    (ja, x) = fa (Jet a [])
    js' = Jet s' (fmap (Fld (Proxy @f)) ja.velocity <> js.velocity)

fldJet :: forall f s a.
  ( HasField' f s a
  , KnownSymbol f
  , Typeable a
  ) =>
  Jet s -> Jet a
fldJet js = Jet (getField @f js.position) (fltc js.velocity) where
  fltc :: [Edit s] -> [Edit a]
  fltc [] = []
  fltc ((Fld p e) : xs)
    | Just e' <- chkfld p e = e' : fltc xs
    | otherwise = fltc xs
  fltc (_ : xs) = fltc xs

  chkfld :: forall f' a'.
    ( HasField' f' s a'
    , KnownSymbol f'
    , Typeable a'
    ) =>
    Proxy f' -> Edit a' -> Maybe (Edit a)
  chkfld _ e
    | Just Refl <- eqT @f' @f
    , Just Refl <- eqT @a' @a = Just e
    | otherwise = Nothing

embedFld :: forall f s a x.
  ( HasField' f s a
  , KnownSymbol f
  , Typeable a
  ) =>
  BuilderM a x ->
  BuilderM s x
embedFld b = BuilderM $ StateT $ \l -> Identity (x, (Embed f g l':l)) where
  (x, l') = runIdentity $ runStateT b.unBuilderM []
  f = fldModifierFn @f
  g = fldJet @f

------------------
-- BUILDING DOM --
------------------

el :: Text -> [(Text, Text)] -> BuilderM s a -> BuilderM s a
el tagName attrs children = do
  let (res, layout) = runIdentity $ runStateT children.unBuilderM []
  modify (ElemNode tagName attrs layout :)
  return res

text :: Text -> BuilderM s ()
text content = modify (TextNode content :)

dynText :: (s -> Text) -> BuilderM s ()
dynText f = modify (DynText f :)

div_ :: [(Text, Text)] -> BuilderM s a -> BuilderM s a
div_ = el "div"

canvas_ :: [(Text, Text)] -> BuilderM s a -> BuilderM s a
canvas_ = el "div"

span_ :: [(Text, Text)] -> BuilderM s a -> BuilderM s a
span_ = el "div"

h1_ :: [(Text, Text)] -> BuilderM s a -> BuilderM s a
h1_ = el "h1"

button_ :: [(Text, Text)] -> BuilderM s a -> BuilderM s a
button_ = el "button"

select_ :: [(Text, Text)] -> BuilderM s a -> BuilderM s a
select_ = el "select"

on :: Text -> (JSVal -> TransitionM s ()) -> BuilderM s ()
on e l = modify (EventListener e l :)

-----------------------------
-- STARTING AN APPLICATION --
-----------------------------

build ::
  ModifierFn s ->
  [Layout s] ->
  s ->
  JSVal ->
  SyncPlan s ->
  IO (SyncPlan s)
build m = go0 . List.reverse where
  go0 [] _ _ p = return p
  go0 (x:xs) s r p = do { p' <- go1 m x s r p; go0 xs s r p' }

  go1 :: forall s.
    ModifierFn s ->
    Layout s ->
    s ->
    JSVal ->
    SyncPlan s ->
    IO (SyncPlan s)
  go1 m (Embed f g l) s r p = do
    p' <- build (f m) l ((g (Jet s [])).position) r []
    return $ fmap (MapOp g) p' <> p
  go1 m (TextNode t) s r p =
    insertText r t >> return p
  go1 m (DynText f) s r p = do
    textNode <- liftIO $ insertText r $ f s
    return $ TextOp textNode f : p
  go1 m (ElemNode tagName attrs children) s r p = do
    el <- insertElement r tagName
    forM_ attrs \(k, v) -> setProperty el k v
    -- TODO: Could use insertBefore() to avoid reversing the list!
    foldM (\p h -> go1 m h s el p) p $ List.reverse children
  go1 m (EventListener eventName listenr) s r p = do
    jlisnr <- js_dynExport \event -> flip runReaderT m $ unTransitionM $ listenr event
    addEventListener r eventName jlisnr
    return p

sync :: SyncPlan s -> Jet s -> IO ()
sync [] _ = return ()
sync (x:xs) s = go x s >> sync xs s where
  go :: forall s. SyncOp s -> Jet s -> IO ()
  go (MapOp f op0) s = do
    let t = f s
    unless (List.null t.velocity) $ go op0 t
  go (TextOp textNode f) s = updateTextContent textNode $ f s.position
  go (PropOp elemNode propName f) s = setProperty elemNode propName $ f s.position

attach :: s -> BuilderM s a -> IO a
attach s html = mdo
  stateRef <- newIORef s
  body <- documentBody
  let (res, layout) = runIdentity $ runStateT html.unBuilderM []
  let mod = refModifier (sync plan) stateRef
  plan <- build mod layout s body []
  return res
