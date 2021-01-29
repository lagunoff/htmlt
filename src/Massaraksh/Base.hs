{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.Reader
import Control.Exception
import Data.Coerce
import Data.Default
import Data.Foldable
import Data.IORef
import Data.JSString.Text as JSS
import Data.List as L
import Data.Text as T hiding (index)
import Language.Javascript.JSaddle as JS
import Massaraksh.DOM
import Massaraksh.Decode
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: Text -> Html x -> Html x
el tag child = do
  elm <- liftJSM (createElement tag)
  localElement elm child

el' :: Text -> Html x -> Html Node
el' tag child = do
  elm <- liftJSM (createElement tag)
  elm <$ localElement elm child

nsEl :: Text -> Text -> Html x -> Html x
nsEl ns tag child = do
  elm <- liftJSM $ createElementNS ns tag
  localElement elm child

text :: Text -> Html ()
text txt = do
  textNode <- liftJSM (createTextNode txt)
  mutateRoot (flip appendChild textNode)

dynText :: Dynamic Text -> Html ()
dynText d = do
  txt <- liftIO (dynamic_read d)
  js <- askJSM
  textNode <- liftJSM (createTextNode txt)
  dynamic_updates d `htmlSubscribe` \new -> void $ liftIO do
    flip runJSM js $ setTextValue textNode new
  mutateRoot (flip appendChild textNode)

prop :: ToJSVal v => Text -> v -> Html ()
prop (JSS.textToJSString -> key) val = mutateRoot \rootEl -> do
  v <- toJSVal val
  unsafeSetProp key v (coerce rootEl)

(=:) :: Text -> Text -> Html ()
(=:) = prop
infixr 3 =:
{-# INLINE (=:) #-}

dynProp :: (ToJSVal v, FromJSVal v, Eq v) => Text -> Dynamic v -> Html ()
dynProp (JSS.textToJSString -> key) dyn = do
  mutate <- askMutateRoot
  let
    setup txt rootEl = toJSVal txt
      >>= flip (unsafeSetProp key) (coerce rootEl)
  void $ forDyn dyn (liftIO . mutate . setup)

(~:) :: (ToJSVal v, FromJSVal v, Eq v) => Text -> Dynamic v -> Html ()
(~:) = dynProp
infixr 3 ~:
{-# INLINE (~:) #-}

attr :: Text -> Text -> Html ()
attr k v = mutateRoot \e -> setAttribute e k v

dynAttr :: Text -> Dynamic Text -> Html ()
dynAttr k d = do
  mutate <- askMutateRoot
  let setup v e = setAttribute e k v
  void $ forDyn d (liftIO . mutate . setup)

on :: Text -> Decoder (Html x) -> Html ()
on name decoder = do
  env <- ask
  mutateRoot \rootEl ->
    liftIO $ runHtml env $ domEvent rootEl name decoder

on_ :: Text -> Html x -> Html ()
on_ name w = on name (pure w)

domEventOpts :: ListenOpts -> Node -> Text -> Decoder (Html x) -> Html ()
domEventOpts opts elm name decoder = do
  env <- ask
  js <- askJSM
  let
    event :: Event (Html ())
    event = Event \s k -> liftIO $ flip runJSM js do
      unlisten <- addEventListener opts elm name \event -> do
        e <- runDecoder decoder event
        either (\_ -> pure ()) (void . liftIO . sync . k . void) e
      pure $ liftIO $ runJSM unlisten js
  void $ htmlSubscribe event (liftIO . runHtml env)

domEvent :: Node -> Text -> Decoder (Html x) -> Html ()
domEvent = domEventOpts def

domEvent_ :: Node -> Text -> Html x -> Html ()
domEvent_ e n act = domEvent e n (pure act)

classes :: Text -> Html ()
classes cs = do
  mutate <- askMutateRoot
  liftIO $ mutate \rootEl -> do
    for_ (T.splitOn (T.pack " ") cs) \c -> do
      classListAdd rootEl c

toggleClass :: Text -> Dynamic Bool -> Html ()
toggleClass cs dyn = do
  mutate <- askMutateRoot
  let
    setup cs enable rootEl = case enable of
      True  -> classListAdd rootEl cs
      False -> classListRemove rootEl cs
  void $ forDyn dyn (liftIO . mutate . setup cs)

toggleAttr :: Text -> Dynamic Bool -> Html ()
toggleAttr att dyn = do
  mutate <- askMutateRoot
  let
    setup name enable rootEl = case enable of
      True -> setAttribute rootEl name (T.pack "on")
      False -> removeAttribute rootEl name
  void $ forDyn dyn (liftIO . mutate . setup att)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

htmlLocal :: (HtmlEnv -> HtmlEnv) -> Html x -> Html x
htmlLocal f (Html (ReaderT h)) = Html $ ReaderT (h . f)

data ItemEnv a = ItemEnv
  { cenvHtmlEnv :: HtmlEnv
  , cenvRef :: DynRef a
  , cenvModify :: Modifier a
  }

itraverseHtml
  :: forall s a
   . IndexedTraversal' Int s a
  -> DynRef s
  -> (Int -> DynRef a -> Html ())
  -> Html ()
itraverseHtml l dynRef h = do
  hte <- ask
  js <- askJSM
  rootEl <- askElement
  s <- readRef dynRef
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ItemEnv a] -> [a] -> [a] -> IO ()
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
            { htenvSubscriptions = subscriptions
            , htenvPostBuild = postRef }
          itemRef = ItemEnv newEnv elemRef' (dynRef_modifier elemRef)
        runHtml newEnv $ h idx elemRef'
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
        liftIO $ sync $ cenvModify r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _) -> do
        error "itraverseHtml: Incoherent internal state"

    unsub = traverse_ \ItemEnv{..} -> do
      subscriptions <- liftIO . readIORef . htenvSubscriptions $ cenvHtmlEnv
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

dyn_ :: Dynamic (Html ()) -> Html ()
dyn_ dyn = do
  env <- ask
  js <- askJSM
  childRef <- liftIO (newIORef Nothing)
  mutate <- askMutateRoot
  let
    unsub newEnv = do
      oldEnv <- readIORef childRef
      for_ oldEnv \HtmlEnv{..} -> do
        subs <- readIORef htenvSubscriptions
        for_ subs (readIORef >=> id)
        writeIORef htenvSubscriptions []
      writeIORef childRef newEnv
    setup html rootEl = liftIO do
      postHooks <- newIORef []
      subscriptions <- newIORef []
      (elmRef, flush) <- flip runJSM js $ deferMutations (htenvElement env)
      let
        newEnv = env
          {htenvSubscriptions=subscriptions, htenvPostBuild=postHooks, htenvElement=elmRef}
        triggerPost = runHtml newEnv . sequence_
          =<< readIORef postHooks
        commit = do
          unsub (Just newEnv)
            <* removeAllChilds env
            <* flush
            <* triggerPost
      runHtml newEnv html <* commit
    removeAllChilds env = mutate \rootEl -> do
      length <- childLength rootEl
      for_ [0..length - 1] \idx -> do
        childEl <- getChildNode rootEl (length - idx - 1)
        removeChild rootEl childEl
  addFinalizer (unsub Nothing)
  void $ forDyn dyn (liftIO . mutate . (void .) . setup)

catchInteractive :: Html () -> (SomeException -> Html ()) -> Html ()
catchInteractive html handle = ask >>= run where
  run e = htmlLocal (f e) html
  f e he = he{htenvCatchInteractive=runHtml e . handle}
