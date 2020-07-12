{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Coerce
import Unsafe.Coerce
import Data.Foldable
import Data.IORef
import Data.List as L
import Data.JSString as T hiding (index)
import Language.Javascript.JSaddle as JS
import Massaraksh.DOM
import Massaraksh.Decode
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: JSString -> Html x -> Html x
el tag child = do
  elm <- liftJSM (createElement tag)
  localElement elm child

el' :: JSString -> Html x -> Html Element
el' tag child = do
  elm <- liftJSM (createElement tag)
  elm <$ localElement elm child

elNS :: JSString -> JSString -> Html x -> Html x
elNS ns tag child = do
  elm <- liftJSM $ createElementNS ns tag
  localElement elm child

text :: JSString -> Html ()
text txt = do
  textNode <- liftJSM (createTextNode txt)
  mutateRoot (flip appendChild textNode)

dynText :: Dynamic JSString -> Html ()
dynText d = do
  txt <- liftIO (dyn_read d)
  js <- askJSM
  textNode <- liftJSM (createTextNode txt)
  dyn_updates d `htmlSubscribe` \new -> void $ liftIO do
    flip runJSM js $ textNode <# "nodeValue" $ new
  mutateRoot (flip appendChild textNode)

prop :: ToJSVal v => JSString -> v -> Html ()
prop key val = mutateRoot \rootEl -> do
  v <- toJSVal val
  unsafeSetProp key v (coerce rootEl)

(=:) :: JSString -> JSString -> Html ()
(=:) = prop
infixr 3 =:
{-# INLINE (=:) #-}

dynProp :: (ToJSVal v, FromJSVal v, Eq v) => JSString -> Dynamic v -> Html ()
dynProp key dyn = do
  mutate <- askMutateRoot
  let
    setup txt rootEl = toJSVal txt
      >>= flip (unsafeSetProp key) (coerce rootEl)
  void $ forDyn dyn (liftIO . mutate . setup)

(~:) :: (ToJSVal v, FromJSVal v, Eq v) => JSString -> Dynamic v -> Html ()
(~:) = dynProp
infixr 3 ~:
{-# INLINE (~:) #-}

attr :: JSString -> JSString -> Html ()
attr key val = mutateRoot \rootEl -> do
  void $ rootEl # "setAttribute" $ (key, val)

dynAttr :: JSString -> Dynamic JSString -> Html ()
dynAttr key dyn = do
  mutate <- askMutateRoot
  let
    setup val rootEl =
      void $ rootEl # "setAttribute" $ (key, val)
  void $ forDyn dyn (liftIO . mutate . setup)

on :: JSString -> Decoder (Html x) -> Html ()
on name decoder = do
  env <- ask
  mutateRoot \rootEl ->
    liftIO $ runHtml env $ onEvent rootEl name decoder

on_ :: JSString -> Html x -> Html ()
on_ name w = on name (pure w)

onEvent :: Element -> JSString -> Decoder (Html x) -> Html ()
onEvent elm name decoder = do
  env <- ask
  js <- askJSM
  let
    event :: Event (Html ())
    event = Event \s k -> liftIO $ flip runJSM js do
      cb <- function $ fun \_ _ [event] -> do
        e <- runDecoder decoder event
        either (\_ -> pure ()) (void . liftIO . sync . k . void) e
      makeCb <- eval ("(function(f) { return function(e){e.preventDefault(); f(e); }; }) ")
      cb' <- call makeCb jsUndefined $ [cb]
      (elm # "addEventListener" $ (name, cb'))
      pure $ liftIO $ flip runJSM js do
        elm # "removeEventListener" $ (name, cb')
        void (freeFunction cb)
  void $ htmlSubscribe event (liftIO . runHtml env)

onEvent_ :: Element -> JSString -> Html x -> Html ()
onEvent_ elm name w = onEvent elm name (pure w)

toggleClass :: JSString -> Dynamic Bool -> Html ()
toggleClass cs dyn = do
  mutate <- askMutateRoot
  let
    setup name enable rootEl = case enable of
      True  -> void $ rootEl ! "classList" # "add" $ [name]
      False -> void $ rootEl ! "classList" # "remove" $ [name]
  void $ forDyn dyn (liftIO . mutate . setup cs)

toggleAttr :: JSString -> Dynamic Bool -> Html ()
toggleAttr cs dyn = do
  mutate <- askMutateRoot
  let
    setup name enable rootEl = case enable of
      True  -> void $ rootEl # "setAttribute" $ (name, "on")
      False -> void $ rootEl # "removeAttribute" $ [name]
  void $ forDyn dyn (liftIO . mutate . setup cs)

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

htmlLocal :: (HtmlEnv -> HtmlEnv) -> Html x -> Html x
htmlLocal f (Html (ReaderT h)) = Html $ ReaderT (h . f)

data ChildHtmlRef s = ChildHtmlRef
  { childHtmlRef_htmlEnv :: HtmlEnv
  , childHtmlRef_dynRef  :: DynamicRef s
  , childHtmlRef_subscriptions :: IORef [IORef (IO ())]
  , childHtmlRef_modify  :: Modifier s
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
  s <- liftIO $ dyn_read (fst dynRef)
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ChildHtmlRef a] -> [a] -> [a] -> IO ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        (subscriber, subscriptions) <- newSubscriber
        dynRef' <- liftIO (newDyn x)
        let
          model   = (fst dynRef', mkModifier idx (fst dynRef'))
          newEnv  = hte
            { he_subscribe  = subscriber
            , he_post_build = error "post hook not implemented" }
          itemRef = ChildHtmlRef newEnv model subscriptions (snd dynRef')
        runHtml newEnv $ h idx model
        liftIO (modifyIORef itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, [])  -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        liftIO (writeIORef itemRefs newRefs)
        for_ tailRefs \ChildHtmlRef{..} -> do
          subscriptions <- liftIO $ readIORef childHtmlRef_subscriptions
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- flip runJSM js $ rootEl ! "childNodes" JS.!! idx
          flip runJSM js (rootEl # "removeChild" $ [childEl])
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ sync $ childHtmlRef_modify r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _)      -> do
        error "dynList: Incoherent internal state"

    mkModifier :: Int -> Dynamic a -> (a -> a) -> Reactive ()
    mkModifier idx dyn f = do
      oldA <- liftIO $ dyn_read dyn
      snd dynRef \oldS ->
        oldS & iover l \i x -> if i == idx then f oldA else x

  liftIO $ setup s 0 [] [] (toListOf l s)
  let eUpdates = withOld s (dyn_updates dyn)
  htmlSubscribe eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    liftIO $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

dynHtml :: Dynamic (Html ()) -> Html ()
dynHtml dyn = dynHtml' $ fmap (\h c _ -> h *> c) dyn

data X

dynHtml' :: Dynamic (Html X -> Html X -> Html X) -> Html ()
dynHtml' dyn = do
  env <- ask
  js <- askJSM
  childRef <- liftIO (newIORef Nothing)
  mutate <- askMutateRoot
  let
    setup html rootEl = liftIO mdo
      postHooks <- newIORef []
      (subscriber, subscriptions) <- newSubscriber
      (elmRef, flush) <- flip runJSM js $ newElementRef' (he_element env)
      let
        unsub = liftIO do
          oldEnv <- readIORef childRef
          for_ oldEnv \(e, s) -> do
            subs <- readIORef s
            for_ subs (readIORef >=> id)
            writeIORef s []
          writeIORef childRef (Just (newEnv, subscriptions))
        newEnv = env
          {he_subscribe=subscriber, he_post_build=postHooks, he_element=elmRef}
      runHtml newEnv do
        let
          commit::Html X = unsafeCoerce ()
            <$ unsub
            <* (sequence_ =<< liftIO (readIORef postHooks))
            <* liftIO (removeAllChilds env)
            <* liftIO (liftIO flush)
          revert::Html X = unsafeCoerce () <$ pure ()
        html commit revert
    removeAllChilds env = mutate \rootEl -> do
      length <- fromJSValUnchecked =<< rootEl ! "childNodes" ! "length"
      for_ [0..length - 1] \idx -> do
        childEl <- rootEl ! "childNodes" JS.!! (length - idx - 1)
        rootEl # "removeChild" $ [childEl]
  void $ forDyn dyn (liftIO . mutate . (void .) . setup)
