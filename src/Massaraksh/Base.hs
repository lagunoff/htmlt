{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Base where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Lens hiding ((#))
import Control.Natural hiding ((#))
import Data.Maybe
import Data.String
import Data.List as L
import Data.Text as T
import Data.Typeable (Typeable)
import Data.IORef
import Language.Javascript.JSaddle
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Pipes as P
import Pipes.Core as P
import Pipes.Internal as P
import Unsafe.Coerce
import qualified Data.Dynamic as D

newtype HtmlT w s t m a = HtmlT { runHtmlT :: ReaderT (HtmlEnv w s t m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (HtmlEnv w s t m), MonadFix)

type HtmlT' w s = HtmlT w s s

type Html w s t m = HtmlT w s t m ()

type Html' w s m = HtmlT w s s m ()

data HtmlEnv w s t m = HtmlEnv
  { hteRootRef    :: RootElmRef
  , hteDynamicRef :: DynamicRef s t
  , hteSubscriber :: Subscriber (ComponentT w s t m ()) }

data RootElmRef = RootElmRef
  { relmRead  :: IO JSVal
  , relmWrite :: JSVal -> IO ()  }

data Subscriber a = Subscriber
  { sbscrPrivate :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
  , sbscrPublic  :: Event a -> IO (IO ()) }

type ComponentT w s t m = Producer1 w (HtmlT w s t m)

type Producer1 w = Proxy X () D.Dynamic (Exists w)

data Exists (f :: * -> *) = forall x. Typeable x => Exists { unExists :: f x }

type MonadHtmlBase m = (MonadJSM m, MonadUnliftIO m)

type IsHtml w s t m h = (MonadHtmlBase m, h ~ HtmlT w s t m)

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
    subscriber = contramap (overComponent ww stab) (hteSubscriber e)
  in ab $ e { hteDynamicRef = dynRef, hteSubscriber = subscriber }

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

newRootRef :: MonadHtmlBase m => HtmlT w s t m RootElmRef
newRootRef = do
  rootEl <- askRootElm
  elRef <- liftIO (newIORef Nothing)
  UnliftIO{..} <- lift askUnliftIO
  let
    initial   = error "Root element was accessed before it was initialized"
    relmRead  = fromMaybe initial <$> readIORef elRef
    relmWrite = \newEl -> do
      readIORef elRef >>= \case
        Nothing    -> unliftIO $ liftJSM (rootEl # "appendChild" $ newEl)
        Just oldEl -> unliftIO $ liftJSM (rootEl # "replaceChild" $ (newEl, oldEl))
      writeIORef elRef (Just newEl)
  pure RootElmRef{..}

data SubscriberRef a = SubscriberRef
  { sbrefValue         :: Subscriber a
  , sbrefSubscriptions :: IORef [IORef (IO ())] }

newSubscriberRef :: (a -> IO ()) -> IO (SubscriberRef a)
newSubscriberRef k = do
  sbrefSubscriptions <- newIORef []
  let
    sbscrPrivate :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
    sbscrPrivate = \e f -> do
      unsub <- e `subscribe` f
      unRef <- newIORef unsub
      modifyIORef sbrefSubscriptions ((:) unRef)
      pure $ modifyIORef sbrefSubscriptions (delete unRef)
    sbscrPublic = \e -> do
      unsub <- e `subscribe` k
      unRef <- newIORef unsub
      modifyIORef sbrefSubscriptions ((:) unRef)
      pure $ modifyIORef sbrefSubscriptions (delete unRef)
    sbrefValue = Subscriber{..}
  pure SubscriberRef{..}

askRootElm :: MonadHtmlBase m => HtmlT w s t m JSVal
askRootElm =
  asks (relmRead . hteRootRef) >>= liftIO

putRootElm :: MonadHtmlBase m => JSVal -> Html w s t m
putRootElm el = do
  RootElmRef{..} <- asks hteRootRef
  liftIO (relmWrite el)

appendChild :: MonadHtmlBase m => JSVal -> Html w s t m
appendChild elm = do
  hteRootRef@RootElmRef{..} <- newRootRef
  liftIO (relmWrite elm)

withAppendChild
  :: MonadHtmlBase m
  => JSVal
  -> HtmlT w s t m a
  -> HtmlT w s t m a
withAppendChild elm child = do
  hteRootRef@RootElmRef{..} <- newRootRef
  liftIO (relmWrite elm)
  local (\env -> env { hteRootRef }) child

subscribePrivate
  :: MonadHtmlBase m
  => Event a
  -> (a -> Html w s t m)
  -> HtmlT w s t m (IO ())
subscribePrivate e f = do
  subscriber <- asks (sbscrPrivate . hteSubscriber)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO . f)

subscribePublic
  :: MonadHtmlBase m
  => Event (ComponentT w s t m ())
  -> HtmlT w s t m (IO ())
subscribePublic e = do
  subscriber <- asks (sbscrPublic . hteSubscriber)
  UnliftIO{..} <- askUnliftIO
  liftIO (subscriber e)

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HtmlT w s t m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (Proxy a' a b' b m) where
  liftJSM' = lift . liftJSM'
#endif

instance MonadHtmlBase m => MonadState s (HtmlT w s s m) where
  get = liftIO =<< asks (dynRead . drefValue . hteDynamicRef)
  put v = liftIO =<< asks (($ const v) . drefModify . hteDynamicRef)

instance MonadUnliftIO m => MonadUnliftIO (HtmlT w s t m) where
  askUnliftIO = HtmlT do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlT)

class Monad m => MonadSplitState s t m | m -> s t where
  sGet :: m s
  sModify :: (s -> t) -> m ()
  sModify f = sGet >>= sPut . f
  sPut :: t -> m ()
  sPut = sModify . const

instance MonadHtmlBase m => MonadSplitState s t (HtmlT w s t m) where
  sGet = liftIO =<< asks (dynRead . drefValue . hteDynamicRef)
  sPut v = liftIO =<< asks (($ const v) . drefModify . hteDynamicRef)

instance MonadTrans (HtmlT w s t) where
  lift = HtmlT . lift

instance (a ~ (), MonadHtmlBase m) => IsString (HtmlT w s t m a) where
  fromString = text . T.pack

instance (Semigroup a, Applicative m) => Semigroup (HtmlT w s t m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT w s t m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Contravariant Subscriber where
  contramap g Subscriber{..} = Subscriber sbscrPrivate (sbscrPublic . fmap g)
