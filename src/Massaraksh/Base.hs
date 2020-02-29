{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Base where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Lens hiding ((#))
import Data.Foldable
import Data.String
import Data.IORef
import Data.List as L
import Data.Text as T hiding (index)
import Language.Javascript.JSaddle as JS
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: HtmlBase m => Text -> HtmlT s m x -> HtmlT s m x
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  localElement elm child

text :: HtmlBase m => Text -> HtmlT s m ()
text txt = do
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild textNode

dynText :: HtmlBase m => (s -> Text) -> HtmlT s m ()
dynText f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  dynUpdates `subscribePrivate` \Update{..} -> do
    void $ liftJSM $ textNode <# "nodeValue" $ f updNew
  appendChild textNode

prop :: (HtmlBase m, ToJSVal v) => Text -> v -> HtmlT s m ()
prop key val = do
  rootEl <- readElement
  liftJSM $ rootEl <# key $ val

(=:) :: HtmlBase m => Text -> Text -> HtmlT s m ()
(=:) = prop
infixr 7 =:

dynProp
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> (s -> v) -> HtmlT s m ()
dynProp key f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  rootEl <- readElement
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} -> liftJSM do
    oldProp <- fromJSValUnchecked =<< rootEl ! key
    when (f updNew /= oldProp) $
      liftJSM (rootEl <# key $ f updNew)

(~:)
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> (s -> v) -> HtmlT s m ()
(~:) = dynProp
infixr 7 ~:

attr :: HtmlBase m => Text -> Text -> HtmlT s m ()
attr key val = do
  rootEl <- readElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: HtmlBase m => Text -> (s -> Text) -> HtmlT s m ()
dynAttr key f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  rootEl <- readElement
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} ->
    void $ liftJSM $ rootEl # "setAttribute" $ (key, f updNew)

on
  :: HtmlBase m
  => Text
  -> Decoder (HtmlT s m x)
  -> HtmlT s m ()
on name decoder = do
  el <- readElement
  UnliftIO{..} <- askUnliftIO
  let
    event = Event \k -> do
      cb <- unliftIO $ liftJSM $ function \_ _ [event] -> do
        runDecoder decoder event >>= \case
          Left err  -> pure ()
          Right val -> liftIO (k val)
      unliftIO $ liftJSM (el # "addEventListener" $ (name, cb))
      pure $ unliftIO $ void $ liftJSM $ el # "removeEventListener" $ (name, cb)
  void $ subscribePublic event

on'
  :: HtmlBase m
  => Text
  -> HtmlT s m x
  -> HtmlT s m ()
on' name w = on name (pure w)

dynClassList :: HtmlBase m => [(Text, s -> Bool)] -> HtmlT s m ()
dynClassList xs =
  dynProp (T.pack "className") $
  \s -> T.unwords (L.foldl' (\acc (cs, f) -> if f s then cs:acc else acc) [] xs)

classList :: HtmlBase m => [(Text, Bool)] -> HtmlT s m ()
classList xs =
  prop (T.pack "className") $
  T.unwords (L.foldl' (\acc (cs, cond) -> if cond then cs:acc else acc) [] xs)

htmlFix :: (HtmlEval w s m -> HtmlEval w s m) -> HtmlEval w s m
htmlFix f = f (htmlFix f)

overHtml
  :: Functor m
  => Lens' s a
  -> HtmlT a m x
  -> HtmlT s m x
overHtml stab = localHtmlEnv \e -> let
  model = overDynamic stab (hteModel e)
  subscriber = flip contramap (hteSubscriber e) \(Exist html) ->
    Exist (overHtml stab html)
  in e { hteModel = model, hteSubscriber = subscriber }

composeHtml
  :: HtmlRec w a m
  -> HtmlRec w a m
  -> HtmlRec w a m
composeHtml next override yield = next (override yield)

interleaveHtml
  :: HtmlBase m
  => Lens' s a
  -> (HtmlLift s t a b m -> HtmlT a m x)
  -> HtmlT s m x
interleaveHtml stab interleave = do
  UnliftIO{..} <- askUnliftIO
  overHtml stab $ interleave (liftIO . unliftIO)

localHtmlEnv
  :: (HtmlEnv s₁ m -> HtmlEnv s₂ m)
  -> HtmlT s₂ m x
  -> HtmlT s₁ m x
localHtmlEnv f (HtmlT (ReaderT h)) = HtmlT $ ReaderT (h . f)

dynListSimple
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> HtmlT a m ()
  -> HtmlT s m ()
dynListSimple l child =
  dynList l $ const (const child)

dynList
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> (Int -> HtmlInterleave s (s, a) m ())
  -> HtmlT s m ()
dynList stbb interleave = do
  unliftH <- askUnliftIO
  unliftM <- lift askUnliftIO
  hte <- ask
  rootEl <- liftIO $ relmRead (hteElement hte)
  s <- liftIO $ dynRead $ drefValue (hteModel hte)
  childEnvs <- liftIO (newIORef [])
  let
    mkHandler :: HtmlEnv a m -> Exist (HtmlT a m) -> IO ()
    mkHandler env (Exist html) =
      void $ unliftIO unliftM $ flip runReaderT env $ runHtmlT html

    setup :: Int -> [a] -> [a] -> m ()
    setup idx old new = case (old, new) of
      ([], [])   -> pure ()
      ([], x:xs) -> mdo
        -- New list is longer, append new elements
        subscriber <- liftIO (newSubscriberRef (mkHandler newEnv))
        let parentDyn = drefValue (hteModel hte)
        -- FIXME:
        value <- liftIO $ mapMaybeD x ((^? stbb . index idx) . updNew) parentDyn
        let
          model = DynamicRef value \ab -> drefModify (hteModel hte) $
            iover stbb \i -> if i == idx then ab else id
          child = interleave idx (liftIO . unliftIO unliftH)
          newEnv = HtmlEnv (hteElement hte) model subscriber
        flip runReaderT newEnv $ runHtmlT child
        liftIO (modifyIORef childEnvs (<> [newEnv]))
        setup (idx + 1) [] xs
      (x:xs, []) -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        childEnvsValue <- liftIO (readIORef childEnvs)
        let (newEnvs, tailEnvs) = L.splitAt idx childEnvsValue
        liftIO (writeIORef childEnvs newEnvs)
        for_ tailEnvs \HtmlEnv{..} -> do
          subscriptions <- liftIO $ readIORef (sbrefSubscriptions hteSubscriber)
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- liftJSM $ rootEl ! "childNodes" JS.!! idx
          liftJSM (rootEl # "removeChild" $ [childEl])
      (x:xs, y:ys) -> do
        setup (idx + 1) xs ys

  HtmlT $ lift $ setup 0 [] (toListOf stbb s)
  let updates = dynUpdates . drefValue . hteModel $ hte
  updates `subscribePrivate` \Update{..} -> do
    HtmlT $ lift $ setup 0 (toListOf stbb updOld) (toListOf stbb updNew)
  pure ()

instance (x ~ (), HtmlBase m) => IsString (HtmlT s m x) where
  fromString = text . T.pack
