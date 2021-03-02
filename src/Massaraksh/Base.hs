module Massaraksh.Base where

import Control.Exception
import Control.Lens hiding ((#))
import Control.Monad.Reader
import Data.Coerce
import Data.Default
import Data.Foldable
import Data.IORef
import Data.JSString.Text as JSS
import Data.List as L
import Data.Text as T hiding (index)
import GHC.Generics
import Language.Javascript.JSaddle as JS
import Massaraksh.DOM
import Massaraksh.Decode
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: Text -> HtmlT x -> HtmlT x
el tag child = do
  elm <- liftJSM (createElement tag)
  withElement elm child

el' :: Text -> HtmlT x -> HtmlT Node
el' tag child = do
  elm <- liftJSM (createElement tag)
  elm <$ withElement elm child

elNs :: Text -> Text -> HtmlT x -> HtmlT x
elNs ns tag child = do
  elm <- liftJSM $ createElementNS ns tag
  withElement elm child

text :: Text -> HtmlT ()
text txt = do
  textNode <- liftJSM (createTextNode txt)
  mutateRoot (flip appendChild textNode)

dynText :: Dynamic Text -> HtmlT ()
dynText d = do
  txt <- liftIO (dynamic_read d)
  js <- askJSM
  textNode <- liftJSM (createTextNode txt)
  dynamic_updates d `htmlSubscribe` \new -> void $ liftIO do
    flip runJSM js $ setTextValue textNode new
  mutateRoot (flip appendChild textNode)

prop :: ToJSVal v => Text -> v -> HtmlT ()
prop (JSS.textToJSString -> key) val = mutateRoot \rootEl -> do
  v <- toJSVal val
  unsafeSetProp key v (coerce rootEl)

dynProp :: (ToJSVal v, FromJSVal v, Eq v) => Text -> Dynamic v -> HtmlT ()
dynProp (JSS.textToJSString -> key) dyn = do
  mutate <- askMutateRoot
  let
    setup txt rootEl = toJSVal txt
      >>= flip (unsafeSetProp key) (coerce rootEl)
  void $ forDyn dyn (liftIO . mutate . setup)

attr :: Text -> Text -> HtmlT ()
attr k v = mutateRoot \e -> setAttribute e k v

dynAttr :: Text -> Dynamic Text -> HtmlT ()
dynAttr k d = do
  mutate <- askMutateRoot
  let setup v e = setAttribute e k v
  void $ forDyn d (liftIO . mutate . setup)

on :: Text -> Decoder (HtmlT x) -> HtmlT ()
on name decoder = do
  env <- ask
  mutateRoot \rootEl ->
    liftIO $ runHtmlT env $ domEvent rootEl name decoder

on_ :: Text -> HtmlT x -> HtmlT ()
on_ name w = on name (pure w)

domEventOpts :: ListenOpts -> Node -> Text -> Decoder (HtmlT x) -> HtmlT ()
domEventOpts opts elm name decoder = do
  env <- ask
  js <- askJSM
  let
    event :: Event (HtmlT ())
    event = Event \s k -> liftIO $ flip runJSM js do
      unlisten <- addEventListener opts elm name \event -> do
        e <- runDecoder decoder event
        maybe blank (void . liftIO . sync . k . void) e
      pure $ liftIO $ runJSM unlisten js
  void $ htmlSubscribe event (liftIO . runHtmlT env)

domEvent :: Node -> Text -> Decoder (HtmlT x) -> HtmlT ()
domEvent = domEventOpts def

domEvent_ :: Node -> Text -> HtmlT x -> HtmlT ()
domEvent_ e n act = domEvent e n (pure act)

classes :: Text -> HtmlT ()
classes cs = mutateRoot \rootEl -> do
  for_ (T.splitOn (T.pack " ") cs) $
    classListAdd rootEl

toggleClass :: Text -> Dynamic Bool -> HtmlT ()
toggleClass cs dyn = do
  mutate <- askMutateRoot
  let
    setup cs enable rootEl = case enable of
      True  -> classListAdd rootEl cs
      False -> classListRemove rootEl cs
  void $ forDyn dyn (liftIO . mutate . setup cs)

toggleAttr :: Text -> Dynamic Bool -> HtmlT ()
toggleAttr att dyn = do
  mutate <- askMutateRoot
  let
    setup name enable rootEl = case enable of
      True -> setAttribute rootEl name (T.pack "on")
      False -> removeAttribute rootEl name
  void $ forDyn dyn (liftIO . mutate . setup att)

blank :: Applicative m => m ()
blank = pure ()

data ElemEnv a = ElemEnv
  { elemEnv_htmlEnv :: HtmlEnv
  , elemEnv_Ref :: DynRef a
  , elemEnv_modifier :: Modifier a
  }
  deriving stock Generic

itraverseHtml
  :: forall s a
  . IndexedTraversal' Int s a
  -> DynRef s
  -> (Int -> DynRef a -> HtmlT ())
  -> HtmlT ()
itraverseHtml l dynRef h = do
  hte <- ask
  js <- askJSM
  rootEl <- askElement
  s <- readRef dynRef
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ElemEnv a] -> [a] -> [a] -> IO ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        subscriptions <- newIORef []
        elemRef <- newRef x
        postRef <- liftIO (newIORef [])
        let
          elemRef' = elemRef {dynRef_modifier=mkModifier idx (fromRef elemRef)}
          newEnv = hte
            { htmlEnv_finalizers = subscriptions
            , htmlEnv_postHooks = postRef }
          itemRef = ElemEnv newEnv elemRef' (dynRef_modifier elemRef)
        runHtmlT newEnv $ h idx elemRef'
        liftIO (modifyIORef itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, []) -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        unsub tailRefs
        childEl <- flip runJSM js $ getChildNode rootEl idx
        flip runJSM js (removeChild rootEl childEl)
        liftIO (writeIORef itemRefs newRefs)
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ sync $ elemEnv_modifier r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _) -> do
        error "itraverseHtml: Incoherent internal state"

    unsub = traverse_ \ElemEnv{..} -> do
      subscriptions <- liftIO . readIORef . htmlEnv_finalizers $ elemEnv_htmlEnv
      liftIO $ for_ subscriptions (readIORef >=> id)

    mkModifier :: Int -> Dynamic a -> (a -> a) -> Reactive ()
    mkModifier idx dyn f = do
      oldA <- liftIO $ dynamic_read dyn
      dynRef_modifier dynRef \oldS ->
        oldS & iover l \i x -> if i == idx then f oldA else x
  liftIO $ setup s 0 [] [] (toListOf l s)
  addFinalizer $ readIORef itemRefs >>= unsub
  let eUpdates = withOld s (dynamic_updates $ fromRef dynRef)
  htmlSubscribe eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    liftIO $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

dyn_ :: Dynamic (HtmlT ()) -> HtmlT ()
dyn_ dyn = do
  env <- ask
  js <- askJSM
  childRef <- liftIO (newIORef Nothing)
  mutate <- askMutateRoot
  let
    unsub newEnv = do
      oldEnv <- readIORef childRef
      for_ oldEnv \HtmlEnv{..} -> do
        subs <- readIORef htmlEnv_finalizers
        for_ subs (readIORef >=> id)
        writeIORef htmlEnv_finalizers []
      writeIORef childRef newEnv
    setup html rootEl = liftIO do
      postHooks <- newIORef []
      subscriptions <- newIORef []
      (elmRef, flush) <- deferMutations (htmlEnv_element env)
      let
        newEnv = env
          { htmlEnv_finalizers = subscriptions
          , htmlEnv_postHooks = postHooks
          , htmlEnv_element = elmRef }
        triggerPost = runHtmlT newEnv . sequence_
          =<< readIORef postHooks
        commit = do
          unsub (Just newEnv)
            <* removeAllChilds env
            <* flush
            <* triggerPost
      runHtmlT newEnv html <* commit
    removeAllChilds env = mutate \rootEl -> do
      length <- childLength rootEl
      for_ [0..length - 1] \idx -> do
        childEl <- getChildNode rootEl (length - idx - 1)
        removeChild rootEl childEl
  addFinalizer (unsub Nothing)
  void $ forDyn dyn (liftIO . mutate . (void .) . setup)

catchInteractive :: HtmlT () -> (SomeException -> HtmlT ()) -> HtmlT ()
catchInteractive html handle = ask >>= run where
  run e = local (f e) html
  f e he = he {htmlEnv_catchInteractive = runHtmlT e . handle}
