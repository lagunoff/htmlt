{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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

type Html e = HtmlM e ()

data DomBuilder = DomBuilder
  { dobAskRoot     :: IO JSVal
  , dobReplaceRoot :: JSVal -> IO ()
  }

data HtmlEnv d w = HtmlEnv
  { hteTellEvent :: TellEvent w
  , hteSubscribe :: Subscribe
  , hteBuilder   :: DomBuilder
  , hteDynamic   :: DynamicHandle d
  , hteLiftJSM   :: LiftJSM }

newtype Subscribe = Subscribe { fromSubscribe :: forall a. Event a -> (a -> IO ()) -> IO () }
newtype TellEvent w = TellEvent { fromTellEvent :: Event w -> IO () }
newtype LiftJSM = LiftJSM { fromLiftJSM :: forall x. JSM x -> IO x }

type HasDom e =
  ( HasDomBuilder e
  , HasLiftJSM e )

type HasModel d e =
  ( HasDom e
  , HasDynamicHandle d e
  , HasSubscribe e )

type HasMessage w e =
  ( HasDom e
  , HasTellEvent w e )

type HasInteractive p w e =
  ( HasModel p e
  , HasTellEvent w e )

newBuilder :: (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m) => m (DomBuilder)
newBuilder = do
  rootEl <- askRoot
  elRef <- liftIO (newIORef Nothing)
  UnliftIO{..} <- askUnliftIO
  let
    dobAskRoot           = fromMaybe (error "root element was accessed before it was initialized") <$> readIORef elRef
    dobReplaceRoot newEl = do
      readIORef elRef >>= \case
        Nothing    -> unliftIO $ liftJSM $ rootEl # "appendChild" $ newEl
        Just oldEl -> unliftIO $ liftJSM $ rootEl # "replaceChild" $ (newEl, oldEl)
      liftIO $ writeIORef elRef (Just newEl)
  pure DomBuilder{..}

askRoot :: forall m e. (MonadReader e m, HasDomBuilder e, MonadIO m) => m JSVal
askRoot = asks (view $ domBuilderL . to dobAskRoot) >>= liftIO

replaceRoot :: forall m e. (MonadReader e m, HasDomBuilder e, MonadIO m) => JSVal -> m ()
replaceRoot el = do
  DomBuilder{..} <- asks (^. domBuilderL)
  liftIO (dobReplaceRoot el)

appendChild' :: forall m e a. (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m) => JSVal -> m a -> m a
appendChild' el child = do
  hteBuilder@DomBuilder{..} <- newBuilder
  liftIO (dobReplaceRoot el)
  local (set domBuilderL hteBuilder) child

appendChild :: forall m e. (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m) => JSVal -> m ()
appendChild el = do
  DomBuilder{..} <- newBuilder
  liftIO (dobReplaceRoot el)

instance (a ~ (), HasDom e) => IsString (HtmlM e a) where
  fromString = text . T.pack

instance Semigroup a => Semigroup (HtmlM e a) where
  (<>) = liftM2 (<>)

type MonadWidget e m = (MonadReader e m, MonadJSM m, HasDom e, Applicative m, MonadIO m, MonadUnliftIO m)

el :: HasDom e => Text -> HtmlM e a -> HtmlM e a
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  appendChild' elm child

prop
  :: forall val key e
   . (HasDom e, ToJSString key, ToJSVal val)
  => key -> val -> Html e
prop key val =
  askRoot >>= \el -> liftJSM $ el <# key $ val

dynProp
  :: forall val key w e
   . (HasDom e, HasModel w e, ToJSString key, ToJSVal val)
  => key -> (w -> val) -> Html e
dynProp key f = do
  Dynamic{..} <- asks (^. dynamicHandleL . to _dhDynamic)
  txt <- f <$> liftIO _dynRead
  el <- askRoot
  liftJSM (el <# key $ txt)
  _dynUpdates `subscribe` \Update{..} ->
    liftJSM (el <# key $ f _updNew)

subscribe :: (MonadReader e m, HasSubscribe e, MonadUnliftIO m) => Event w -> (w -> m ()) -> m ()
subscribe e f = do
  subscribe_ <- asks (^. subscribeL . to fromSubscribe)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscribe_` (unliftIO . f)

on :: (HasDom e, HasMessage w e) => Text -> Decoder w -> Html e
on name decoder = do
  builder <- asks (^. domBuilderL)
  tellEvent <- asks (^. tellEventL . to fromTellEvent)
  el <- askRoot
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

on1 :: (HasDom e, HasMessage w e) => Text -> w -> Html e
on1 name w = on name (pure w)

(=:) :: HasDom e => Text -> Text -> Html e
(=:) = prop

(~:) :: (HasDom e, HasModel w e) => Text -> (w -> Text) -> Html e
(~:) = dynProp

infixr 7 ~:, =:

text :: HasDom e => Text -> Html e
text txt = do
  el <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild el

dynText :: (HasDom e, HasModel w e) => (w -> Text) -> Html e
dynText f = do
  Dynamic{..} <- asks (^. dynamicHandleL . to _dhDynamic)
  txt <- f <$> liftIO _dynRead
  el <- liftJSM $ jsg "document" # "createTextNode" $ txt
  _dynUpdates `subscribe` \Update{..} -> do
    void $ liftJSM $ el <# "nodeValue" $ f _updNew
  appendChild el

type Producer1 w = Proxy X () D.Dynamic (Exists w)

data Exists (f :: * -> *) = forall x. Typeable x => Exists { unExists :: f x }

yield1 :: (Monad m, Typeable a) => w a -> Producer1 w m a
yield1 w = (\(D.Dynamic _ x) -> unsafeCoerce x) <$> respond (Exists w)

class HasDomBuilder e where
  domBuilderL :: Lens' e DomBuilder

class HasDynamicHandle d e | e -> d where
  dynamicHandleL :: Lens' e (DynamicHandle d)

class HasTellEvent w e | e -> w where
  tellEventL :: Lens' e (TellEvent w)

class HasSubscribe e where
  subscribeL :: Lens' e Subscribe

class HasLiftJSM e where
  liftJSML :: Lens' e LiftJSM

#ifndef ghcjs_HOST_OS
instance (HasLiftJSM e) => MonadJSM (HtmlM e) where
  liftJSM' jsm = asks (^. liftJSML . to fromLiftJSM) >>= liftIO . ($ jsm)
#endif

instance (HasDynamicHandle d e) => MonadState d (HtmlM e) where
  get = liftIO =<< asks (^. dynamicHandleL . to _dhDynamic . to _dynRead)
  put v = liftIO =<< asks (^. dynamicHandleL . to _dhModify . to ($ const v))

instance MonadUnliftIO (HtmlM e) where
  askUnliftIO = HtmlM do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlM)

class HasHtmlEnv d w e | e -> d w where
  htmlEnvL :: Lens' e (HtmlEnv d w)

instance HasHtmlEnv d w (HtmlEnv d w) where
  htmlEnvL = iso id id

instance HasHtmlEnv d w e => HasDomBuilder e where
  domBuilderL = htmlEnvL . lens hteBuilder \s b -> s { hteBuilder = b }

instance HasHtmlEnv d w e => HasDynamicHandle d e where
  dynamicHandleL = htmlEnvL . lens hteDynamic \s b -> s { hteDynamic = b }

instance HasHtmlEnv d w e => HasTellEvent w e where
  tellEventL = htmlEnvL . lens hteTellEvent \s b -> s { hteTellEvent = b }

instance HasHtmlEnv d w e => HasSubscribe e where
  subscribeL = htmlEnvL . lens hteSubscribe \s b -> s { hteSubscribe = b }

instance HasHtmlEnv d w e => HasLiftJSM e where
  liftJSML = htmlEnvL . lens hteLiftJSM \s b -> s { hteLiftJSM = b }
