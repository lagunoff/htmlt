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
import Data.Typeable (Typeable)
import Language.Javascript.JSaddle
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types
import Pipes.Core as P
import Pipes.Internal as P
import Unsafe.Coerce
import qualified Data.Dynamic as D

el :: MonadHtmlBase m => Text -> HtmlT w s t m a -> HtmlT w s t m a
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  withAppendChild elm child

text :: MonadHtmlBase m => Text -> Html w s t m
text txt = do
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild textNode

dynText :: MonadHtmlBase m => (s -> Text) -> Html w s t m
dynText f = do
  Dynamic{..} <- asks (drefValue . hteDynamicRef)
  txt <- f <$> liftIO dynRead
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  dynUpdates `subscribePrivate` \Update{..} -> do
    void $ liftJSM $ textNode <# "nodeValue" $ f updNew
  appendChild textNode

prop
  :: forall v w s t m
   . (MonadHtmlBase m, ToJSVal v)
  => Text -> v -> Html w s t m
prop key val = do
  rootEl <- askRootElm
  liftJSM $ rootEl <# key $ val

(=:) :: MonadHtmlBase m => Text -> Text -> Html w s t m
(=:) = prop
infixr 7 =:

dynProp
  :: forall v w s t m
   . (MonadHtmlBase m, ToJSVal v)
  => Text -> (s -> v) -> Html w s t m
dynProp key f = do
  Dynamic{..} <- asks (drefValue . hteDynamicRef)
  txt <- f <$> liftIO dynRead
  rootEl <- askRootElm
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} ->
    liftJSM (rootEl <# key $ f updNew)

(~:) :: (MonadHtmlBase m, ToJSVal v) => Text -> (s -> v) -> Html w s t m
(~:) = dynProp
infixr 7 ~:

attr :: MonadHtmlBase m => Text -> Text -> Html w s t m
attr key val = do
  rootEl <- askRootElm
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: MonadHtmlBase m => Text -> (s -> Text) -> Html w s t m
dynAttr key f = do
  Dynamic{..} <- asks (drefValue . hteDynamicRef)
  txt <- f <$> liftIO dynRead
  rootEl <- askRootElm
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} ->
    void $ liftJSM $ rootEl # "setAttribute" $ (key, f updNew)

on
  :: MonadHtmlBase m
  => Text
  -> Decoder (ComponentT w s t m ())
  -> Html w s t m
on name decoder = do
  el <- askRootElm
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
  :: MonadHtmlBase m
  => Text
  -> ComponentT w s t m ()
  -> Html w s t m
on' name w = on name (pure w)

yieldOn
  :: MonadHtmlBase m
  => Text
  -> Decoder (w ())
  -> Html w s t m
yieldOn name decoder = on name (fmap yield1 decoder)

yieldOn'
  :: MonadHtmlBase m
  => Text
  -> w ()
  -> Html w s t m
yieldOn' name w = on name (pure (yield1 w))

dynClassList :: MonadHtmlBase m => [(Text, s -> Bool)] -> Html w s t m
dynClassList xs =
  dynProp (T.pack "className") $
  \s -> T.unwords (L.foldl' (\acc (cs, f) -> if f s then cs:acc else acc) [] xs)

classList :: MonadHtmlBase m => [(Text, Bool)] -> Html w s t m
classList xs =
  prop (T.pack "className") $
  T.unwords (L.foldl' (\acc (cs, cond) -> if cond then cs:acc else acc) [] xs)

overHtml
  :: Functor m
  => (w' ~> w)
  -> Lens s t a b
  -> HtmlT w' a b m x
  -> HtmlT w s t m x
overHtml ww stab (HtmlT (ReaderT ab)) = HtmlT $ ReaderT \e ->
  let
    dynRef = overDynamic stab (hteDynamicRef e)
    subscriber = contramap (overComponent ww stab) (hteSubscriberRef e)
  in ab $ e { hteDynamicRef = dynRef, hteSubscriberRef = subscriber }

overComponent
  :: Functor m
  => (w' ~> w)
  -> Lens s t a b
  -> ComponentT w' a b m x
  -> ComponentT w s t m x
overComponent ww stab = \case
  Request q k          -> Request q \a' -> overComponent ww stab (k a')
  Respond (Exists w) k -> Respond (Exists (ww w)) \a' -> overComponent ww stab (k a')
  M m                  -> M (overHtml ww stab m <&> overComponent ww stab)
  Pure r               -> Pure r

htmlLocal
  :: (HtmlEnv w s t m -> HtmlEnv w' s' t' m)
  -> HtmlT w' s' t' m x
  -> HtmlT w s t m x
htmlLocal f (HtmlT (ReaderT mx)) = HtmlT $ ReaderT (mx . f)

componentLocal
  :: Monad m
  => (HtmlEnv w s t m -> HtmlEnv w' s' t' m)
  -> (w' ~> ComponentT w' s' t' m)
  -> ComponentT w' s' t' m x
  -> ComponentT w s t m x
componentLocal f ww = \case
  Request q k          -> Request q \a' -> componentLocal f ww (k a')
  Respond (Exists w) k -> componentLocal f ww (ww w) >>= componentLocal f ww . k . D.toDyn
  M m                  -> M (htmlLocal f m <&> componentLocal f ww)
  Pure r               -> Pure r

yield1 :: (Monad m, Typeable a) => w a -> Producer1 w m a
yield1 w = (\(D.Dynamic _ x) -> unsafeCoerce x) <$> respond (Exists w)

dynList
  :: forall w w' s a m
   . MonadHtmlBase m
  => IndexedTraversal' Int s a
  -> (HtmlEnv w' a a m -> (w' ~> w))
  -> Html w' a a m
  -> Html w s s m
dynList stbb liftw child = do
  hte <- ask
  rootEl <- liftIO $ relmRead (hteRootRef hte)
  s <- liftIO $ dynRead $ drefValue (hteDynamicRef hte)
  childEnvs <- liftIO (newIORef [])
  let
    setup :: Int -> [a] -> [a] -> m ()
    setup idx old new = case (old, new) of
      ([], [])   -> pure ()
      ([], x:xs) -> mdo
        -- New list is longer, appending new elements
        hteSubscriberRef <- liftIO (newSubscriberRef undefined)
        hteDynamicRef <- liftIO (newDynamicRef x)
        let newEnv = HtmlEnv{hteRootRef = hteRootRef hte, ..}
        flip runReaderT newEnv $ runHtmlT child
        liftIO (modifyIORef childEnvs (<> [newEnv]))
        setup (idx + 1) [] xs
      (x:xs, []) -> do
        -- New list is shorter, deleting elements that no longer
        -- present in the new list
        childEnvsValue <- liftIO (readIORef childEnvs)
        let tailEnvs = L.drop idx childEnvsValue
        for_ tailEnvs \HtmlEnv{..} -> do
          subscriptions <- liftIO $ readIORef (sbrefSubscriptions hteSubscriberRef)
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- liftIO (relmRead hteRootRef)
          liftJSM (rootEl # "removeChild" $ childEl)
      (x:xs, y:ys) -> do
        setup (idx + 1) xs ys

  HtmlT $ lift $ setup 0 [] (toListOf stbb s)
  let updates = dynUpdates . drefValue . hteDynamicRef $ hte
  updates `subscribePrivate` \Update{..} -> do
    HtmlT $ lift $ setup 0 (toListOf stbb updOld) (toListOf stbb updNew)
  pure ()

instance (a ~ (), MonadHtmlBase m) => IsString (HtmlT w s t m a) where
  fromString = text . T.pack
