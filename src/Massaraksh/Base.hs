{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Bool
import Data.Coerce
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

el :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m x
el tag child = do
  elm <- liftJSM (createElement tag)
  localElement elm (child <* flushFragment)

elNS :: HtmlBase m => Maybe JSString -> JSString -> HtmlT m x -> HtmlT m x
elNS ns tag child = do
  elm <- liftJSM $ maybe (createElement tag) (flip createElementNS tag) ns
  localElement elm (child <* flushFragment)

el' :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m Element
el' tag child = do
  elm <- liftJSM (createElement tag)
  elm <$ localElement elm (child <* flushFragment)

text :: HtmlBase m => JSString -> HtmlT m ()
text txt = do
  textNode <- liftJSM (createTextNode txt)
  withNewChild textNode <* flushFragment

dynText :: HtmlBase m => Dynamic JSString -> HtmlT m ()
dynText d = do
  txt <- liftIO (dyn_read d)
  un <- askUnliftIO
  textNode <- liftJSM (createTextNode txt)
  dyn_updates d `htmlSubscribe` \new -> do
    void $ liftIO $ unliftIO un $ liftJSM $ textNode <# "nodeValue" $ new
  withNewChild textNode <* flushFragment

prop :: (ToJSVal v, HtmlBase m) => JSString -> v -> HtmlT m ()
prop key val = do
  rootEl <- askElement
  liftJSM do
    v <- toJSVal val
    unsafeSetProp key v (coerce rootEl)

(=:) :: HtmlBase m => JSString -> JSString -> HtmlT m ()
(=:) = prop
infixr 3 =:

dynProp
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => JSString -> Dynamic v -> HtmlT m ()
dynProp key dyn = do
  txt <- liftIO (dyn_read dyn)
  rootEl <- askElement
  liftJSM do
    v <- toJSVal txt
    unsafeSetProp key v (coerce rootEl)
  un <- askUnliftIO
  void $ dyn_updates dyn `htmlSubscribe` \new -> void $ liftIO $ unliftIO un $ liftJSM do
    v <- toJSVal new
    unsafeSetProp key v (coerce rootEl)

(~:)
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => JSString -> Dynamic v -> HtmlT m ()
(~:) = dynProp
infixr 3 ~:

attr :: HtmlBase m => JSString -> JSString -> HtmlT m ()
attr key val = do
  rootEl <- askElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: HtmlBase m => JSString -> Dynamic JSString -> HtmlT m ()
dynAttr key dyn = do
  txt <- liftIO (dyn_read dyn)
  rootEl <- askElement
  liftJSM (rootEl <# key $ txt)
  un <- askUnliftIO
  void $ dyn_updates dyn `htmlSubscribe` \new ->
    void $ liftIO $ unliftIO un $ liftJSM $ rootEl # "setAttribute" $ (key, new)

on :: HtmlBase m => JSString -> Decoder (HtmlT m x) -> HtmlT m ()
on name decoder = do
  el <- askElement
  un <- askUnliftIO
  onEvent el name $ fmap (liftIO . unliftIO un) decoder

on_ :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m ()
on_ name w = on name (pure w)

onEvent :: HtmlBase m => Element -> JSString -> Decoder (HtmlT m x) -> HtmlT m ()
onEvent elm name decoder = do
  un <- askUnliftIO
  let
    event = Event \s k -> liftIO $ unliftIO un $ liftJSM do
      cb <- function $ fun \_ _ [event] -> do
        runDecoder decoder event >>= either (\_ -> pure ()) (liftIO . sync . k)
      -- f <- eval ("function(t,n,c) {t.addEventListener(n, function(e){ e.preventDefault(); c(e):)}"::JSString)
      (elm # "addEventListener" $ (name, cb))
      pure $ void $ liftIO $ unliftIO un $ liftJSM do
        elm # "removeEventListener" $ (name, cb)
        freeFunction cb
  void $ htmlSubscribe event (void . liftIO . unliftIO un)

onEvent_ :: HtmlBase m => Element -> JSString -> HtmlT m x -> HtmlT m ()
onEvent_ elm name w = onEvent elm name (pure w)

dynClassList :: HtmlBase m => [(JSString, Dynamic Bool)] -> HtmlT m ()
dynClassList xs = do
  rootEl <- askElement
  un <- askUnliftIO
  let
    setup name = \case
      True  -> void $ liftJSM (rootEl ! "classList" # "add" $ [name])
      False -> void $ liftJSM (rootEl ! "classList" # "remove" $ [name])
  for_ xs \(name, dyn) -> do
    setup name =<< liftIO (dyn_read dyn)
    subscribeUpdates dyn (liftIO . unliftIO un . setup name)

classList :: HtmlBase m => [(JSString, Bool)] -> HtmlT m ()
classList xs =
  prop (T.pack "className") $ T.unwords
    $ L.foldl' (\acc (cs, cond) -> bool acc (cs:acc) cond) [] xs

-- TODO: consider alternative to dynClassList
-- toggleClass :: HtmlBase m => JSString -> Dynamic Bool -> HtmlT m ()

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

htmlLocal
  :: HtmlBase m
  => (HtmlEnv m -> HtmlEnv m)
  -> HtmlT m x
  -> HtmlT m x
htmlLocal f (HtmlT (ReaderT h)) = HtmlT $ ReaderT (h . f)

data ChildHtmlRef s m = ChildHtmlRef
  { childHtmlRef_htmlEnv :: HtmlEnv m
  , childHtmlRef_dynRef  :: DynamicRef s
  , childHtmlRef_subscriptions :: IORef [IORef (IO ())]
  , childHtmlRef_modify  :: Modifier s
  }

itraverseHtml
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> DynamicRef s
  -> (Int -> DynamicRef a -> HtmlT m ())
  -> HtmlT m ()
itraverseHtml l dynRef@(dyn, _) h = do
  unliftH <- askUnliftIO
  unliftM <- lift askUnliftIO
  hte <- ask
  rootEl <- askElement
  s <- liftIO $ dyn_read (fst dynRef)
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ChildHtmlRef a m] -> [a] -> [a] -> m ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        (subscriber, subscriptions) <- liftIO newSubscriber
        dynRef' <- liftIO (newDyn x)
        let
          model   = (fst dynRef', mkModifier idx (fst dynRef'))
          newEnv  = HtmlEnv (he_element hte) subscriber (error "post hook not implemented")
          itemRef = ChildHtmlRef newEnv model subscriptions (snd dynRef')
        runHtmlT newEnv $ h idx model
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
          childEl <- liftJSM $ rootEl ! "childNodes" JS.!! idx
          liftJSM (rootEl # "removeChild" $ [childEl])
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

  lift $ setup s 0 [] [] (toListOf l s)
  let eUpdates = withOld s (dyn_updates dyn)
  htmlSubscribe eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    liftIO $ unliftIO unliftM $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

dynHtml :: HtmlBase m => Dynamic (HtmlT m ()) -> HtmlT m ()
dynHtml dyn = do
  un <- lift askUnliftIO
  env <- ask
  childRef <- liftIO (newIORef Nothing)
  let
    setup html = mdo
      oldEnv <- liftIO (readIORef childRef)
      for_ oldEnv \(e, s) -> liftIO do
        subs <- readIORef s
        for_ subs (readIORef >=> id)
        writeIORef s []
      (subscriber, subscriptions) <- liftIO newSubscriber
      postHooks <- liftIO $ newIORef []
      let newEnv = env {he_subscribe=subscriber, he_post_build=postHooks}
      liftIO $ writeIORef childRef (Just (newEnv, subscriptions))
      runHtmlT newEnv (html <* removeAllChilds <* flushFragment)
      liftIO (readIORef postHooks) >>= mapM_ (runHtmlT newEnv)

    -- FIXME: that limits usage of 'dynHtml' to the cases when it is
    -- the only children of some HTML element because all sibling will
    -- be removed when tag is switched. Alternative way could be: keep
    -- track of the elements that were appended during initialization
    -- and remove only them
    removeAllChilds = askElement >>= \rootEl -> liftJSM do
      length <- fromJSValUnchecked =<< rootEl ! "childNodes" ! "length"
      for_ [0..length - 1] \idx -> do
        childEl <- rootEl ! "childNodes" JS.!! (length - idx - 1)
        rootEl # "removeChild" $ [childEl]
  liftIO (dyn_read dyn) >>= lift . setup
  void $ subscribeUpdates dyn (liftIO . unliftIO un . setup)
