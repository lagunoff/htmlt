{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Natural hiding ((#))
import Data.IORef
import Data.Maybe
import Data.String
import Data.Coerce
import Data.Text as T
import Language.Javascript.JSaddle
import Massaraksh.Event
import Massaraksh.DOM

newtype HtmlT m a = HtmlT { runHtmlT' :: ReaderT (HtmlEnv m) m a }
  deriving (
    Functor, Applicative, Monad, MonadIO, MonadReader (HtmlEnv m), MonadFix
  )

data HtmlEnv m = HtmlEnv
  { htmlEnvElement    :: ElementRef
  , htmlEnvSubscriber :: SubscriberRef (Exist (HtmlT m))
  }

data ElementRef = ElementRef
  { elementRefRead  :: IO Element
  , elementRefWrite :: Maybe Element -> IO ()
  }

data Subscriber a = Subscriber
  { subscriberPrivate :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
  , subscriberPublic  :: Event a -> IO (IO ())
  }

data SubscriberRef a = SubscriberRef
  { subscriberRefPrivate       :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
  , subscriberRefPublic        :: Event a -> IO (IO ())
  , subscriberRefSubscriptions :: IORef [IORef (IO ())]
  }

type HtmlEmit w m = (w ~> HtmlT m) -> (w ~> HtmlT m)

data Exist (f :: * -> *) = forall x. Exist (f x)

type HtmlBase m = (MonadJSM m, MonadUnliftIO m, MonadFix m)

runHtmlT :: HtmlEnv m -> HtmlT m x -> m x
runHtmlT e = flip runReaderT e . runHtmlT'

fix1 :: (w ~> m -> w ~> m) -> w ~> m
fix1 f = f (fix1 f)

compose1
  :: (w ~> m -> w ~> m)
  -> (w ~> m -> w ~> m)
  -> w ~> m -> w ~> m
compose1 a b wm = a (b wm)
{-# INLINE compose1 #-}

instance MonadUnliftIO m => MonadUnliftIO (HtmlT m) where
  askUnliftIO = HtmlT do
    un <- askUnliftIO
    pure $ UnliftIO (unliftIO un . runHtmlT')

instance MonadTrans HtmlT where
  lift = HtmlT . lift

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty

instance Contravariant Subscriber where
  contramap g (Subscriber priv pub) =
    Subscriber priv (pub . fmap g)

instance Contravariant SubscriberRef where
  contramap g (SubscriberRef priv pub subs) =
    SubscriberRef priv (pub . fmap g) subs

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HtmlT m) where
  liftJSM' = lift . liftJSM'
#endif

instance (x ~ (), HtmlBase m) => IsString (HtmlT m x) where
  fromString = text . T.pack
    where
      -- FIXME: Duplicated code from other modules
      text :: HtmlBase m => Text -> HtmlT m ()
      text txt = do
        textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
        hteElement <- newElementRef
        liftIO $ elementRefWrite hteElement (Just (coerce textNode))

      askElement :: HtmlBase m => HtmlT m Element
      askElement =
        liftIO =<< asks (elementRefRead . htmlEnvElement)

      newElementRef :: HtmlBase m => HtmlT m ElementRef
      newElementRef = do
        rootEl <- askElement
        elementRef <- liftIO (newIORef Nothing)
        un <- lift askUnliftIO
        let
          initial = error "Root element was accessed before it was initialized"
          read    = fromMaybe initial <$> readIORef elementRef
          write   = \new -> do
            readIORef elementRef >>= \old -> case (old, new) of
              (Nothing, Just newEl)    -> void $ unliftIO un $ liftJSM
                (rootEl # "appendChild" $ newEl)
              (Just oldEl, Just newEl) -> void $ unliftIO un $ liftJSM
                (rootEl # "replaceChild" $ (newEl, oldEl))
              (Just oldEl, Nothing)    -> void $ unliftIO un $ liftJSM
                (rootEl # "removeChild" $ oldEl)
              (Nothing, Nothing)       -> pure ()
            writeIORef elementRef new
        pure (ElementRef read write)
