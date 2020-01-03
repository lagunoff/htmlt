{-# LANGUAGE CPP #-}
module Massaraksh.Base where

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding ((#))
import Data.String
import Data.Text as T
import Data.Maybe
import GHC.IORef
import Language.Javascript.JSaddle
import Pipes as P
import Pipes.Core as P
import Massaraksh.Decode
import Control.Monad.IO.Unlift
import Massaraksh.Event
import Massaraksh.Dynamic
import Unsafe.Coerce

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
  , HasDynamic d e
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
askRoot = asks (view $ hasDomBuilder . to dobAskRoot) >>= liftIO

replaceRoot :: forall m e. (MonadReader e m, HasDomBuilder e, MonadIO m) => JSVal -> m ()
replaceRoot el = do
  DomBuilder{..} <- asks (view hasDomBuilder)
  liftIO (dobReplaceRoot el)

appendChild' :: forall m e a. (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m) => JSVal -> m a -> m a
appendChild' el child = do
  hteBuilder@DomBuilder{..} <- newBuilder
  liftIO (dobReplaceRoot el)
  local (set hasDomBuilder hteBuilder) child

appendChild :: forall m e. (MonadReader e m, HasDomBuilder e, MonadJSM m, MonadUnliftIO m) => JSVal -> m ()
appendChild el = do
  DomBuilder{..} <- newBuilder
  liftIO (dobReplaceRoot el)

instance (a ~ (), HasDom e) => IsString (HtmlM e a) where
  fromString = text . T.pack

instance Semigroup a => Semigroup (HtmlM e a) where
  (<>) a b = liftM2 (<>) a b

type MonadWidget e m = (MonadReader e m, MonadJSM m, HasDom e, Applicative m, MonadIO m, MonadUnliftIO m)

el :: MonadWidget e m => Text -> m a -> m a
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  appendChild' elm child

prop
  :: forall val key e m
   . (MonadWidget e m, ToJSString key, ToJSVal val)
  => key -> val -> m ()
prop key val =
  askRoot >>= \el -> liftJSM $ el <# key $ val

dynProp
  :: forall val key w e m
   . (MonadWidget e m, HasModel w e, ToJSString key, ToJSVal val)
  => key -> (w -> val) -> m ()
dynProp key f = do
  Dynamic{..} <- asks (view $ hasDynamic . to _dhDynamic)
  txt <- f <$> liftIO _dynRead
  el <- askRoot
  liftJSM (el <# key $ txt)
  _dynUpdates `subscribe` \Update{..} ->
    liftJSM (el <# key $ f _updNew)

subscribe :: (MonadReader e m, HasSubscribe e, MonadUnliftIO m) => Event w -> (w -> m ()) -> m ()
subscribe e f = do
  subscribe_ <- asks (view $ hasSubscribe . to fromSubscribe)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscribe_` (unliftIO . f)

on :: (MonadWidget e m, HasMessage w e) => Text -> Decoder w -> m ()
on name decoder = do
  builder <- asks (view hasDomBuilder)
  tellEvent <- asks (view $ hasTellEvent . to fromTellEvent)
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

(=:) :: MonadWidget e m => Text -> Text -> m ()
(=:) = prop

(~:) :: (MonadWidget e m, HasModel w e) => Text -> (w -> Text) -> m ()
(~:) = dynProp

infixr 7 ~:, =:

text :: MonadWidget e m => Text -> m ()
text txt = do
  el <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild el

dynText :: (MonadWidget e m, HasModel w e) => (w -> Text) -> m ()
dynText f = do
  Dynamic{..} <- asks (view $ hasDynamic . to _dhDynamic)
  txt <- f <$> liftIO _dynRead
  el <- liftJSM $ jsg "document" # "createTextNode" $ txt
  _dynUpdates `subscribe` \Update{..} -> do
    void $ liftJSM $ el <# "nodeValue" $ f _updNew
  appendChild el

type Producer1 w = Proxy X () (Existed w) (Exists w)

data Exists (f :: * -> *) = forall x. Exists { unExists :: f x }
data Existed (f :: * -> *) = forall x. Existed { existedF :: f x, existedX :: x }

yield1 :: Monad m => w a -> Producer1 w m a
yield1 w = (\(Existed _ x) -> unsafeCoerce x) <$> respond (Exists w)

class HasDomBuilder a where
  hasDomBuilder :: Lens' a DomBuilder

class HasSubscribe a where
  hasSubscribe :: Lens' a Subscribe

class HasDynamic w a | a -> w where
  hasDynamic :: Lens' a (DynamicHandle w)

class HasTellEvent w a | a -> w where
  hasTellEvent :: Lens' a (TellEvent w)

class HasLiftJSM a where
  hasLiftJSM :: Lens' a LiftJSM

instance HasDomBuilder (HtmlEnv d w) where
  hasDomBuilder = lens hteBuilder \s hteBuilder -> s { hteBuilder }

instance HasDynamic d (HtmlEnv d w) where
  hasDynamic = lens hteDynamic \s hteDynamic -> s { hteDynamic }

instance HasTellEvent w (HtmlEnv d w) where
  hasTellEvent = lens hteTellEvent \s hteTellEvent -> s { hteTellEvent }

instance HasSubscribe (HtmlEnv d w) where
  hasSubscribe = lens hteSubscribe \s hteSubscribe -> s { hteSubscribe }

instance HasLiftJSM (HtmlEnv d w) where
  hasLiftJSM = lens hteLiftJSM \s hteLiftJSM -> s { hteLiftJSM }

#ifndef ghcjs_HOST_OS
instance (HasLiftJSM e) => MonadJSM (HtmlM e) where
  liftJSM' jsm = asks (view $ hasLiftJSM . to fromLiftJSM) >>= liftIO . ($ jsm)
#endif

instance (HasDynamic d e) => MonadState d (HtmlM e) where
  get = liftIO =<< asks (view $ hasDynamic . to _dhDynamic . to _dynRead)
  put v = liftIO =<< asks (view $ hasDynamic . to _dhModify . to ($ const v))

instance MonadUnliftIO (HtmlM e) where
  askUnliftIO = HtmlM do
    UnliftIO{..} <- askUnliftIO
    pure $ UnliftIO (unliftIO . runHtmlM)

