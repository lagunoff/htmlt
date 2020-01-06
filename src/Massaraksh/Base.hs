{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.String
import Data.Text as T
import Data.Typeable (Typeable)
import GHC.IORef
import Language.Javascript.JSaddle
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Pipes as P
import Pipes.Core as P
import Unsafe.Coerce
import qualified Data.Dynamic as D

newtype HtmlM e a = HtmlM { runHtmlM :: ReaderT e IO a }
  deriving (Functor, Applicative, Monad, MonadReader e, MonadIO, MonadFix)

type Html e w s t = HtmlM (HtmlEnv e w s t) ()
type Html' e w s = HtmlM (HtmlEnv e w s s) ()

data DomBuilder = DomBuilder
  { dobRootNode    :: IO JSVal
  , dobReplaceRoot :: JSVal -> IO ()
  }

data HtmlEnv e w s t = HtmlEnv
  { hteDomBuilder  :: DomBuilder
  , hteDynamicRef  :: DynamicRef s t
  , hteExtraEnv    :: e
  , hteEventWriter :: EventWriter (ComponentM e w s t ())
  , hteLiftJSM     :: LiftJSM
  , hteSubscriber  :: Subscriber }

type HtmlEnv' e w s = HtmlEnv e w s s

newtype Subscriber = Subscriber
  { runSubscriber :: forall a. Event a -> (a -> IO ()) -> IO () }

newtype EventWriter w = EventWriter
  { runEventWriter :: Event w -> IO () }

newtype LiftJSM = LiftJSM
  { runLiftJSM :: forall x. JSM x -> IO x }

type ComponentM e w s t = Producer1 w (HtmlM (HtmlEnv e w s t))
type ComponentM' e w s = Producer1 w (HtmlM (HtmlEnv e w s s))

type Producer1 w = Proxy X () D.Dynamic (Exists w)

data Exists (f :: * -> *) = forall x. Typeable x => Exists { unExists :: f x }

yield1 :: (Monad m, Typeable a) => w a -> Producer1 w m a
yield1 w = (\(D.Dynamic _ x) -> unsafeCoerce x) <$> respond (Exists w)

type HasDom e =
  ( HasDomBuilder e
  , HasLiftJSM e )

type HasDynamic s t e =
  ( HasDynamicRef s t e
  , HasSubscriber e )

newBuilder
  :: ( MonadReader e m
     , HasDomBuilder e
     , MonadJSM m
     , MonadUnliftIO m )
  => m (DomBuilder)
newBuilder = do
  rootEl <- askRootNode
  elRef <- liftIO (newIORef Nothing)
  UnliftIO{..} <- askUnliftIO
  let
    initial     = error "Root element was accessed before it was initialized"
    dobRootNode = fromMaybe initial <$> readIORef elRef
    dobReplaceRoot newEl = do
      readIORef elRef >>= \case
        Nothing    -> unliftIO $ liftJSM $ rootEl # "appendChild" $ newEl
        Just oldEl -> unliftIO $ liftJSM $ rootEl # "replaceChild" $ (newEl, oldEl)
      liftIO $ writeIORef elRef (Just newEl)
  pure DomBuilder{..}

askRootNode
  :: forall m e
   . (MonadReader e m, HasDomBuilder e, MonadIO m)
  => m JSVal
askRootNode = asks (^. domBuilderL . to dobRootNode) >>= liftIO

replaceRoot
  :: forall m e
   . (MonadReader e m, HasDomBuilder e, MonadIO m)
  => JSVal -> m ()
replaceRoot el = do
  DomBuilder{..} <- asks (^. domBuilderL)
  liftIO (dobReplaceRoot el)

withAppendChild
  :: forall m e a
   . (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m)
  => JSVal -> m a -> m a
withAppendChild el child = do
  hteBuilder@DomBuilder{..} <- newBuilder
  liftIO (dobReplaceRoot el)
  local (set domBuilderL hteBuilder) child

appendChild
  :: forall m e
   . (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m)
  => JSVal -> m ()
appendChild el = do
  DomBuilder{..} <- newBuilder
  liftIO (dobReplaceRoot el)

instance (a ~ (), HasDom e) => IsString (HtmlM e a) where
  fromString = text . T.pack

instance Semigroup a => Semigroup (HtmlM e a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (HtmlM e a) where
  mempty = HtmlM $ ReaderT \_ -> mempty

el :: HasDom e => Text -> HtmlM e a -> HtmlM e a
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  withAppendChild elm child

prop
  :: forall val key e
   . (HasDom e, ToJSString key, ToJSVal val)
  => key -> val -> HtmlM e ()
prop key val =
  askRootNode >>= \el -> liftJSM $ el <# key $ val

attr :: HasDom e => Text -> Text -> HtmlM e ()
attr key val =
  askRootNode >>= \el -> void $ liftJSM $ el # "setAttribute" $ (key, val)

dynProp
  :: forall v s t e
   . (HasDom e, HasDynamic s t e, ToJSVal v)
  => Text -> (s -> v) -> HtmlM e ()
dynProp key f = do
  Dynamic{..} <- asks (^. dynamicRefL . to drefValue)
  txt <- f <$> liftIO dynRead
  el <- askRootNode
  liftJSM (el <# key $ txt)
  dynUpdates `htmlSubscribe` \Update{..} ->
    liftJSM (el <# key $ f updNew)

htmlSubscribe
  :: (MonadReader e m, HasSubscriber e, MonadUnliftIO m)
  => Event w -> (w -> m ()) -> m ()
htmlSubscribe e f = do
  subscriber <- asks (^. subscriberL . to runSubscriber)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO . f)

on :: (HasDom e, HasEventWriter w e) => Text -> Decoder w -> HtmlM e ()
on name decoder = do
  builder <- asks (^. domBuilderL)
  tellEvent <- asks (^. eventWriterL . to runEventWriter)
  el <- askRootNode
  liftJSM do
    UnliftIO{..} <- askUnliftIO
    let
      event = Event \k -> do
        cb <- unliftIO $ function \_ _ [event] -> do
          runDecoder decoder event >>= \case
            Left err  -> pure ()
            Right val -> liftIO (k val)
        unliftIO (el # "addEventListener" $ (name, cb))
        pure $ unliftIO $ void $ el # "removeEventListener" $ (name, cb)
    liftIO (tellEvent event)

on1 :: (HasDom e, HasEventWriter w e) => Text -> w -> HtmlM e ()
on1 name w = on name (pure w)

on1_
  :: ( HasDom e
     , HasEventWriter (Producer1 w (HtmlM e) ()) e )
  => Text
  -> (Producer1 w (HtmlM e) ())
  -> HtmlM e ()
on1_ name w = on name (pure w)

on_
  :: ( HasDom e
     , HasEventWriter (Producer1 w (HtmlM e) ()) e )
  => Text
  -> Decoder (Producer1 w (HtmlM e) ())
  -> HtmlM e ()
on_ = on

(=:) :: HasDom e => Text -> Text -> HtmlM e ()
(=:) = prop

(~:)
  :: forall v s t e
   . (HasDom e, HasDynamic s t e, ToJSVal v)
  => Text -> (s -> v) -> HtmlM e ()
(~:) = dynProp

infixr 7 ~:, =:

text :: HasDom e => Text -> HtmlM e ()
text txt = do
  el <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild el

dynText :: (HasDom e, HasDynamic s t e) => (s -> Text) -> HtmlM e ()
dynText f = do
  Dynamic{..} <- asks (^. dynamicRefL . to drefValue)
  txt <- f <$> liftIO dynRead
  el <- liftJSM $ jsg "document" # "createTextNode" $ txt
  dynUpdates `htmlSubscribe` \Update{..} -> do
    void $ liftJSM $ el <# "nodeValue" $ f updNew
  appendChild el

class HasDomBuilder e where
  domBuilderL :: Lens' e DomBuilder

class HasDynamicRef s t e | e -> s t where
  dynamicRefL :: Lens' e (DynamicRef s t)

class HasEventWriter w e | e -> w where
  eventWriterL :: Lens' e (EventWriter w)

class HasSubscriber e where
  subscriberL :: Lens' e Subscriber

class HasLiftJSM e where
  liftJSML :: Lens' e LiftJSM

#ifndef ghcjs_HOST_OS
instance (HasLiftJSM e) => MonadJSM (HtmlM e) where
  liftJSM' jsm = asks (^. liftJSML . to runLiftJSM) >>= liftIO . ($ jsm)

instance MonadJSM m => MonadJSM (Proxy a' a b' b m) where
  liftJSM' = lift . liftJSM'
#endif

instance (HasDynamicRef s s e) => MonadState s (HtmlM e) where
  get = liftIO =<< asks (^. dynamicRefL . to drefValue . to dynRead)
  put v = liftIO =<< asks (^. dynamicRefL . to drefModify . to ($ const v))

instance MonadUnliftIO (HtmlM e) where
  askUnliftIO = HtmlM do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlM)

class HasHtmlEnv e w s t a | a -> e w s t where
  htmlEnvL :: Lens' a (HtmlEnv e w s t)

instance HasHtmlEnv e w s t (HtmlEnv e w s t) where
  htmlEnvL = iso id id

instance HasHtmlEnv e w s t a => HasDomBuilder a where
  domBuilderL = htmlEnvL . lens hteDomBuilder \s b -> s { hteDomBuilder = b }

instance HasHtmlEnv e w s t a => HasDynamicRef s t a where
  dynamicRefL = htmlEnvL . lens hteDynamicRef \s b -> s { hteDynamicRef = b }

instance HasHtmlEnv e w s t a => HasEventWriter (ComponentM e w s t ()) a where
  eventWriterL = htmlEnvL . lens hteEventWriter \s b -> s { hteEventWriter = b }

instance HasHtmlEnv e w s t a => HasSubscriber a where
  subscriberL = htmlEnvL . lens hteSubscriber \s b -> s { hteSubscriber = b }

instance HasHtmlEnv e w s t a => HasLiftJSM a where
  liftJSML = htmlEnvL . lens hteLiftJSM \s b -> s { hteLiftJSM = b }

class Monad m => MonadStateW s t m | m -> s t where
  getw :: m s
  modifyw :: (s -> t) -> m ()
  modifyw f = getw >>= putw . f
  putw :: t -> m ()
  putw = modifyw . const

instance HasDynamicRef s t a => MonadStateW s t (HtmlM a) where
  getw = liftIO =<< asks (^. dynamicRefL . to drefValue . to dynRead)
  putw v = liftIO =<< asks (^. dynamicRefL . to drefModify . to ($ const v))

