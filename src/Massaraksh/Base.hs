{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Base where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Lens hiding ((#))
import Control.Natural hiding ((#))
import Data.Foldable
import Data.String
import Data.IORef
import Data.List as L
import Data.Text as T
import Language.Javascript.JSaddle
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: HtmlBase m => Text -> HtmlT s t m x -> HtmlT s t m x
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  localDOMElement elm child

text :: HtmlBase m => Text -> HtmlT s t m ()
text txt = do
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild textNode

dynText :: HtmlBase m => (s -> Text) -> HtmlT s t m ()
dynText f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  dynUpdates `subscribePrivate` \Update{..} -> do
    void $ liftJSM $ textNode <# "nodeValue" $ f updNew
  appendChild textNode

prop :: (HtmlBase m, ToJSVal v) => Text -> v -> HtmlT s t m ()
prop key val = do
  rootEl <- readElement
  liftJSM $ rootEl <# key $ val

(=:) :: HtmlBase m => Text -> Text -> HtmlT s t m ()
(=:) = prop
infixr 7 =:

dynProp :: (HtmlBase m, ToJSVal v, Eq v) => Text -> (s -> v) -> HtmlT s t m ()
dynProp key f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  rootEl <- readElement
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} ->
    when (f updNew /= f updOld) $
      liftJSM (rootEl <# key $ f updNew)

(~:) :: (HtmlBase m, ToJSVal v, Eq v) => Text -> (s -> v) -> HtmlT s t m ()
(~:) = dynProp
infixr 7 ~:

attr :: HtmlBase m => Text -> Text -> HtmlT s t m ()
attr key val = do
  rootEl <- readElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: HtmlBase m => Text -> (s -> Text) -> HtmlT s t m ()
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
  -> Decoder (HtmlT s t m x)
  -> HtmlT s t m ()
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
  -> HtmlT s t m x
  -> HtmlT s t m ()
on' name w = on name (pure w)

dynClassList :: HtmlBase m => [(Text, s -> Bool)] -> HtmlT s t m ()
dynClassList xs =
  dynProp (T.pack "className") $
  \s -> T.unwords (L.foldl' (\acc (cs, f) -> if f s then cs:acc else acc) [] xs)

classList :: HtmlBase m => [(Text, Bool)] -> HtmlT s t m ()
classList xs =
  prop (T.pack "className") $
  T.unwords (L.foldl' (\acc (cs, cond) -> if cond then cs:acc else acc) [] xs)

overHtml
  :: Functor m
  => Lens s t a b
  -> HtmlT a b m x
  -> HtmlT s t m x
overHtml stab (HtmlT (ReaderT ab)) = HtmlT $ ReaderT \e ->
  let
    dynRef = overDynamic stab (hteModel e)
    subscriber = contramap (\(Exist w) -> Exist (overHtml stab w)) (hteSubscriber e)
  in ab $ e { hteModel = dynRef, hteSubscriber = subscriber }

localHtmlEnv
  :: (HtmlEnv s₁ t₁ m -> HtmlEnv s₂ t₂ m)
  -> HtmlT s₂ t₂ m x
  -> HtmlT s₁ t₁ m x
localHtmlEnv f (HtmlT (ReaderT h)) = HtmlT $ ReaderT (h . f)

dynList
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> HtmlT a a m ()
  -> HtmlT s s m ()
dynList stbb child = do
  hte <- ask
  rootEl <- liftIO $ relmRead (hteElement hte)
  s <- liftIO $ dynRead $ drefValue (hteModel hte)
  childEnvs <- liftIO (newIORef [])
  let
    setup :: Int -> [a] -> [a] -> m ()
    setup idx old new = case (old, new) of
      ([], [])   -> pure ()
      ([], x:xs) -> mdo
        -- New list is longer, appending new elements
        hteSubscriber <- liftIO (newSubscriberRef undefined)
        hteModel <- liftIO (newDynamicRef x)
        let newEnv = HtmlEnv{hteElement = hteElement hte, ..}
        flip runReaderT newEnv $ runHtmlT child
        liftIO (modifyIORef childEnvs (<> [newEnv]))
        setup (idx + 1) [] xs
      (x:xs, []) -> do
        -- New list is shorter, deleting elements that no longer
        -- present in the new list
        childEnvsValue <- liftIO (readIORef childEnvs)
        let tailEnvs = L.drop idx childEnvsValue
        for_ tailEnvs \HtmlEnv{..} -> do
          subscriptions <- liftIO $ readIORef (sbrefSubscriptions hteSubscriber)
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- liftIO (relmRead hteElement)
          liftJSM (rootEl # "removeChild" $ childEl)
      (x:xs, y:ys) -> do
        setup (idx + 1) xs ys

  HtmlT $ lift $ setup 0 [] (toListOf stbb s)
  let updates = dynUpdates . drefValue . hteModel $ hte
  updates `subscribePrivate` \Update{..} -> do
    HtmlT $ lift $ setup 0 (toListOf stbb updOld) (toListOf stbb updNew)
  pure ()

instance (a ~ (), HtmlBase m) => IsString (HtmlT s t m ()) where
  fromString = text . T.pack
