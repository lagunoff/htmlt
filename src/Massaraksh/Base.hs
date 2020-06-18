{-# LANGUAGE ViewPatterns #-}
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
  localElement elm child <* flushFragment

el' :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m Element
el' tag child = do
  elm <- liftJSM (createElement tag)
  elm <$ localElement elm child <* flushFragment

text :: HtmlBase m => JSString -> HtmlT m ()
text txt = do
  textNode <- liftJSM (createTextNode txt)
  withNewChild textNode <* flushFragment

dynText :: HtmlBase m => Dyn JSString -> HtmlT m ()
dynText d = do
  txt <- liftIO (readDyn d)
  textNode <- liftJSM (createTextNode txt)
  updates d `subscribePrivate` \new -> do
    void $ liftJSM $ textNode <# "nodeValue" $ new
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
  => JSString -> Dyn v -> HtmlT m ()
dynProp key dyn = do
  txt <- liftIO (readDyn dyn)
  rootEl <- askElement
  liftJSM do
    v <- toJSVal txt
    unsafeSetProp key v (coerce rootEl)
  void $ updates dyn `subscribePrivate` \new -> liftJSM do
    v <- toJSVal new
    unsafeSetProp key v (coerce rootEl)

(~:)
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => JSString -> Dyn v -> HtmlT m ()
(~:) = dynProp
infixr 3 ~:

attr :: HtmlBase m => JSString -> JSString -> HtmlT m ()
attr key val = do
  rootEl <- askElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: HtmlBase m => JSString -> Dyn JSString -> HtmlT m ()
dynAttr key dyn = do
  txt <- liftIO (readDyn dyn)
  rootEl <- askElement
  liftJSM (rootEl <# key $ txt)
  void $ updates dyn `subscribePrivate` \new ->
    void $ liftJSM $ rootEl # "setAttribute" $ (key, new)

on :: HtmlBase m => JSString -> Decoder (HtmlT m x) -> HtmlT m ()
on name decoder = do
  el <- askElement
  onEvent el name decoder

on_ :: HtmlBase m => JSString -> HtmlT m x -> HtmlT m ()
on_ name w = on name (pure w)

onEvent :: HtmlBase m => Element -> JSString -> Decoder (HtmlT m x) -> HtmlT m ()
onEvent elm name decoder = do
  un <- askUnliftIO
  let
    event = Event \k -> do
      cb <- unliftIO un $ liftJSM $ function \_ _ [event] -> do
        runDecoder decoder event >>= either (\_ -> pure ()) (liftIO . k)
      unliftIO un $ liftJSM (elm # "addEventListener" $ (name, cb))
      pure $ void $ unliftIO un $ liftJSM do
        elm # "removeEventListener" $ (name, cb)
        freeFunction cb
  void $ subscribePublic event

onEvent_ :: HtmlBase m => Element -> JSString -> HtmlT m x -> HtmlT m ()
onEvent_ elm name w = onEvent elm name (pure w)

dynClassList :: HtmlBase m => [(JSString, Dyn Bool)] -> HtmlT m ()
dynClassList xs = do
  rootEl <- askElement
  let
    setup name = \case
      True  -> void $ liftJSM (rootEl ! "classList" # "add" $ [name])
      False -> void $ liftJSM (rootEl ! "classList" # "remove" $ [name])
  for_ xs \(name, dyn) -> do
    setup name =<< liftIO (readDyn dyn)
    subscribeUpdates dyn (setup name)

classList :: HtmlBase m => [(JSString, Bool)] -> HtmlT m ()
classList xs =
  prop (T.pack "className") $ T.unwords
    $ L.foldl' (\acc (cs, cond) -> bool acc (cs:acc) cond) [] xs

localHtmlEnv
  :: HtmlBase m
  => (HtmlEnv m -> HtmlEnv m)
  -> HtmlT m x
  -> HtmlT m x
localHtmlEnv f (HtmlT (ReaderT h)) = HtmlT $ ReaderT (h . f)

withDynamicRef
  :: HtmlBase m
  => s
  -> (DynRef s -> HtmlT m x)
  -> HtmlT m x
withDynamicRef initial f =
  liftIO (newDynRef initial) >>= f

data ChildHtmlRef s m = ChildHtmlRef
  { childHtmlRef_htmlEnv :: HtmlEnv m
  , childHtmlRef_dynRef  :: DynRef s
  , childHtmlRef_modify  :: (s -> s) -> IO ()
  }

itraverseHtml
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> DynRef s
  -> (Int -> DynRef a -> HtmlT m ())
  -> HtmlT m ()
itraverseHtml l dynRef@(getDyn -> dyn) h = do
  unliftH <- askUnliftIO
  unliftM <- lift askUnliftIO
  hte <- ask
  rootEl <- askElement
  s <- liftIO (readDynRef dynRef)
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ChildHtmlRef a m] -> [a] -> [a] -> m ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        subscriber <- liftIO $ newSubscriberRef \(Exist h) ->
          void $ unliftIO unliftM $ runHtmlT newEnv h
        dynRef' <- liftIO $ newDynRef x
        let
          model   = dynRef' {dr_modify = mkModifier idx (getDyn dynRef')}
          newEnv  = HtmlEnv (he_element hte) subscriber
          itemRef = ChildHtmlRef newEnv model (modifyDynRef dynRef')
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
          subscriptions <- liftIO $ readIORef $
            subscriberRefSubscriptions (he_subscriber childHtmlRef_htmlEnv)
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- liftJSM $ rootEl ! "childNodes" JS.!! idx
          liftJSM (rootEl # "removeChild" $ [childEl])
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ childHtmlRef_modify r \_ -> y
        setup s (idx + 1) rs xs ys
      (_, _, _)      -> do
        error "dynList: Incoherent internal state"

    mkModifier :: Int -> Dyn a -> (a -> a) -> IO ()
    mkModifier idx dyn f = do
      oldA <- readDyn dyn
      modifyDynRef dynRef \oldS ->
        oldS & iover l \i x -> if i == idx then f oldA else x

  lift $ setup s 0 [] [] (toListOf l s)
  eUpdates <- liftIO $ withOld s (updates dyn)
  subscribePrivate eUpdates \(old, new) -> do
    refs <- liftIO (readIORef itemRefs)
    lift $ setup new 0 refs (toListOf l old) (toListOf l new)
  pure ()

dynHtml :: HtmlBase m => Dyn (HtmlT m ()) -> HtmlT m ()
dynHtml dyn = do
  un <- lift askUnliftIO
  env <- ask
  childRef <- liftIO (newIORef Nothing)
  let
    setup html = mdo
      oldEnv <- liftIO (readIORef childRef)
      for_ oldEnv \e -> liftIO do
        let oldSubsRef = subscriberRefSubscriptions (he_subscriber e)
        subs <- readIORef oldSubsRef
        for_ subs (readIORef >=> id)
        writeIORef oldSubsRef []
      runHtmlT env removeAllChilds
      subscriber <- mkSubscriber newEnv
      let newEnv = env {he_subscriber=subscriber}
      liftIO $ writeIORef childRef (Just newEnv)
      void (runHtmlT newEnv html)

    mkSubscriber = \e -> liftIO $ newSubscriberRef \(Exist h) ->
      void $ unliftIO un $ runHtmlT e h

    -- FIXME: that limits usage of 'dynHtml' to the cases when it is
    -- the only children of some HTML element because all sibling will
    -- be removed when tag is switched. Alternative way could be: keep
    -- track of the elements that were appended during initialization
    -- and remove only them
    removeAllChilds = askElement >>= \rootEl -> liftJSM do
      length <- fromJSValUnchecked =<< rootEl ! "childNodes" ! "length"
      for_ [0..length - 1] \idx -> do
        childEl <- rootEl ! "childNodes" JS.!! idx
        rootEl # "removeChild" $ [childEl]
  liftIO (readDyn dyn) >>= lift . setup
  void $ subscribeUpdates dyn (lift . setup)
