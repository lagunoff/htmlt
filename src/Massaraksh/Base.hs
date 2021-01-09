{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.Reader
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
import Unsafe.Coerce

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
  txt <- liftIO (dnRead d)
  js <- askJSM
  textNode <- liftJSM (createTextNode txt)
  dnUpdates d `htmlSubscribe` \new -> void $ liftIO do
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
  elmJs <- liftJSM (toJSVal elm)
  let
    event :: Event (Html ())
    event = Event \s k -> liftIO $ flip runJSM js do
      unlisten <- addEventListener opts elmJs name \event -> do
        e <- runDecoder decoder event
        either (\_ -> pure ()) (void . liftIO . sync . k . void) e
      pure $ liftIO $ runJSM unlisten js
  void $ htmlSubscribe event (liftIO . runHtml env)

domEvent :: Node -> Text -> Decoder (Html x) -> Html ()
domEvent = domEventOpts def

domEvent_ :: Node -> Text -> Html x -> Html ()
domEvent_ e n act = domEvent e n (pure act)

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
      True  -> setAttribute rootEl name (T.pack "on")
      False -> removeAttribute rootEl name
  void $ forDyn dyn (liftIO . mutate . setup att)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

htmlLocal :: (HtmlEnv -> HtmlEnv) -> Html x -> Html x
htmlLocal f (Html (ReaderT h)) = Html $ ReaderT (h . f)

data ChildrenEnv s = ChildrenEnv
  { cenvHtmlEnv :: HtmlEnv
  , cenvDynRef  :: DynamicRef s
  , cenvSubscriptions :: IORef [IORef (IO ())]
  , cenvModify  :: Modifier s
  }

itraverseHtml
  :: forall s a
   . IndexedTraversal' Int s a
  -> DynamicRef s
  -> (Int -> DynamicRef a -> Html ())
  -> Html ()
itraverseHtml l dynRef@(dyn, _) h = do
  hte <- ask
  js <- askJSM
  rootEl <- askElement
  s <- liftIO $ dnRead (fst dynRef)
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ChildrenEnv a] -> [a] -> [a] -> IO ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        (subscriber, subscriptions) <- newSubscriber
        dynRef' <- liftIO (newDyn x)
        let
          model   = (fst dynRef', mkModifier idx (fst dynRef'))
          newEnv  = hte
            { htnvSubscribe  = subscriber
            , htnvPostBuild = error "post hook not implemented" }
          itemRef = ChildrenEnv newEnv model subscriptions (snd dynRef')
        runHtml newEnv $ h idx model
        liftIO (modifyIORef itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, [])  -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        liftIO (writeIORef itemRefs newRefs)
        for_ tailRefs \ChildrenEnv{..} -> do
          subscriptions <- liftIO $ readIORef cenvSubscriptions
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- flip runJSM js $ getChildNode rootEl idx
          flip runJSM js (removeChild rootEl childEl)
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ sync $ cenvModify r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _)      -> do
        error "dynList: Incoherent internal state"

    mkModifier :: Int -> Dynamic a -> (a -> a) -> Reactive ()
    mkModifier idx dyn f = do
      oldA <- liftIO $ dnRead dyn
      snd dynRef \oldS ->
        oldS & iover l \i x -> if i == idx then f oldA else x

  liftIO $ setup s 0 [] [] (toListOf l s)
  let eUpdates = withOld s (dnUpdates dyn)
  htmlSubscribe eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    liftIO $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

dynHtml :: Dynamic (Html ()) -> Html ()
dynHtml dyn = dynHtml' $ fmap (\h c _ -> h *> c) dyn

dynHtml' :: Dynamic (Html () -> Html () -> Html ()) -> Html ()
dynHtml' dyn = do
  env <- ask
  js <- askJSM
  childRef <- liftIO (newIORef Nothing)
  mutate <- askMutateRoot
  let
    setup html rootEl = liftIO mdo
      postHooks <- newIORef []
      (subscriber, subscriptions) <- newSubscriber
      (elmRef, flush) <- flip runJSM js $ newElementRef' (htnvElement env)
      let
        unsub = liftIO do
          oldEnv <- readIORef childRef
          for_ oldEnv \(e, s) -> do
            subs <- readIORef s
            for_ subs (readIORef >=> id)
            writeIORef s []
          writeIORef childRef (Just (newEnv, subscriptions))
        newEnv = env
          {htnvSubscribe=subscriber, htnvPostBuild=postHooks, htnvElement=elmRef}
      runHtml newEnv do
        let
          commit::Html () = do
            unsub
              <* (sequence_ =<< liftIO (readIORef postHooks))
              <* unless is1 (liftIO removeAllChilds)
              <* liftIO (liftIO commit1)
            pure (unsafeCoerce ())
          revert::Html () = unsafeCoerce () <$ pure ()
        html commit revert
    removeAllChilds env = mutate \rootEl -> do
      length <- childLength rootEl
      for_ [0..length - 1] \idx -> do
        childEl <- getChildNode rootEl (length - idx - 1)
        removeChild rootEl childEl
  void $ forDyn dyn (liftIO . mutate . (void .) . setup)
