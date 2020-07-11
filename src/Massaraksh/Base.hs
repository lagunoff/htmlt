{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Bool
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

elNS :: Maybe JSString -> JSString -> Html x -> Html x
elNS ns tag child = do
  elm <- liftJSM $ maybe (createElement tag) (flip createElementNS tag) ns
  localElement elm child

el' :: JSString -> Html x -> Html Element
el' tag child = do
  elm <- liftJSM (createElement tag)
  elm <$ localElement elm child

text :: JSString -> Html ()
text txt = do
  elm <- askElement
  textNode <- liftJSM (createTextNode txt)
  liftJSM (appendChild elm textNode)

dynText :: Dynamic JSString -> Html ()
dynText d = do
  txt <- liftIO (dyn_read d)
  js <- askJSM
  elm <- askElement
  textNode <- liftJSM (createTextNode txt)
  dyn_updates d `htmlSubscribe` \new -> do
    void $ liftIO $ flip runJSM js $ textNode <# "nodeValue" $ new
  liftJSM (appendChild elm textNode)

prop :: ToJSVal v => JSString -> v -> Html ()
prop key val = do
  rootEl <- askElement
  liftJSM do
    v <- toJSVal val
    unsafeSetProp key v (coerce rootEl)

(=:) :: JSString -> JSString -> Html ()
(=:) = prop
infixr 3 =:

dynProp
  :: (ToJSVal v, FromJSVal v, Eq v)
  => JSString -> Dynamic v -> Html ()
dynProp key dyn = do
  txt <- liftIO (dyn_read dyn)
  rootEl <- askElement
  liftJSM do
    v <- toJSVal txt
    unsafeSetProp key v (coerce rootEl)
  js <- askJSM
  void $ dyn_updates dyn `htmlSubscribe` \new -> void $ liftIO $ flip runJSM js do
    v <- toJSVal new
    unsafeSetProp key v (coerce rootEl)

(~:)
  :: (ToJSVal v, FromJSVal v, Eq v)
  => JSString -> Dynamic v -> Html ()
(~:) = dynProp
infixr 3 ~:

attr :: JSString -> JSString -> Html ()
attr key val = do
  rootEl <- askElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: JSString -> Dynamic JSString -> Html ()
dynAttr key dyn = do
  txt <- liftIO (dyn_read dyn)
  rootEl <- askElement
  liftJSM (rootEl <# key $ txt)
  js <- askJSM
  void $ dyn_updates dyn `htmlSubscribe` \new ->
    void $ liftIO $ flip runJSM js $ rootEl # "setAttribute" $ (key, new)

on :: JSString -> Decoder (Html x) -> Html ()
on name decoder = do
  el <- askElement
  onEvent el name decoder

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

dynClassList :: [(JSString, Dynamic Bool)] -> Html ()
dynClassList xs = do
  rootEl <- askElement
  js <- askJSM
  let
    setup name = \case
      True  -> void $ flip runJSM js (rootEl ! "classList" # "add" $ [name])
      False -> void $ flip runJSM js (rootEl ! "classList" # "remove" $ [name])
  for_ xs \(name, dyn) -> do
    liftIO (setup name =<< dyn_read dyn)
    subscribeUpdates dyn (liftIO . setup name)

classList :: [(JSString, Bool)] -> Html ()
classList xs =
  prop (T.pack "className") $ T.unwords
    $ L.foldl' (\acc (cs, cond) -> bool acc (cs:acc) cond) [] xs

toggleClass :: JSString -> Dynamic Bool -> Html ()
toggleClass cs dyn = do
  rootEl <- askElement
  js <- askJSM
  let
    setup name = \case
      True  -> void $ flip runJSM js (rootEl ! "classList" # "add" $ [name])
      False -> void $ flip runJSM js (rootEl ! "classList" # "remove" $ [name])
  liftIO (setup cs =<< dyn_read dyn)
  void $ subscribeUpdates dyn (liftIO . setup cs)

toggleAttr :: JSString -> Dynamic Bool -> Html ()
toggleAttr cs dyn = do
  rootEl <- askElement
  js <- askJSM
  let
    setup name = \case
      True  -> void $ flip runJSM js (rootEl # "setAttribute" $ (name, "on"))
      False -> void $ flip runJSM js  (rootEl # "removeAttribute" $ [name])
  liftIO (dyn_read dyn >>= setup cs)
  void $ subscribeUpdates dyn (liftIO . setup cs)

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
          newEnv  = HtmlEnv (he_element hte) subscriber (error "post hook not implemented") (he_js_context hte)
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
  let
    setup html = mdo
      postHooks <- liftIO (newIORef [])
      (subscriber, subscriptions) <- liftIO newSubscriber
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
            <* removeAllChilds env
            <* liftIO flush
          revert::Html X = unsafeCoerce () <$ pure ()
        html commit revert
    removeAllChilds env = liftIO (er_read (he_element env))
      >>= \rootEl -> liftJSM do
      length <- fromJSValUnchecked =<< rootEl ! "childNodes" ! "length"
      for_ [0..length - 1] \idx -> do
        childEl <- rootEl ! "childNodes" JS.!! (length - idx - 1)
        rootEl # "removeChild" $ [childEl]
  liftIO (dyn_read dyn >>= setup)
  void $ subscribeUpdates dyn (void . liftIO . setup)
