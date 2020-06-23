{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Types where

import Control.Applicative
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Natural hiding ((#))
import Data.Coerce
import Data.IORef
import Data.JSString as T
import Data.String
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event

newtype HtmlT m a = HtmlT (ReaderT (HtmlEnv m) m a)
  deriving stock Functor
  deriving newtype (
    Applicative, Monad, MonadIO, MonadReader (HtmlEnv m), MonadFix
  )

type HtmlM = HtmlT JSM
type Html = HtmlM ()

data HtmlEnv m = HtmlEnv
  { he_element   :: ElementRef
  , he_subscribe :: Subscriber }

newtype Subscriber = Subscriber
  { sub_unsubscriber ::  forall a. Event a -> Callback a -> Reactive Canceller
  }

type Subscriptions = IORef [IORef (IO ())]

data ElementRef = ElementRef
  { er_read     :: IO Element
  , er_fragment :: IO Fragment
  , er_write    :: Element -> IO () }

type HtmlEmit w m = (w ~> HtmlT m) -> (w ~> HtmlT m)

data Exist (f :: * -> *) = forall x. Exist (f x)

type HtmlBase m = (MonadJSM m, MonadUnliftIO m, MonadFix m)

runHtmlT :: HtmlEnv m -> HtmlT m x -> m x
runHtmlT e (HtmlT h) = runReaderT h e

fix1 :: (w ~> m -> w ~> m) -> w ~> m
fix1 f = f (fix1 f)

compose1
  :: (w ~> m -> w ~> m)
  -> (w ~> m -> w ~> m)
  -> w ~> m -> w ~> m
compose1 a b wm = a (b wm)
{-# INLINE compose1 #-}

instance MonadUnliftIO m => MonadUnliftIO (HtmlT m) where
  askUnliftIO = HtmlT $ ReaderT \e -> do
    un <- askUnliftIO
    pure $ UnliftIO (unliftIO un . runHtmlT e)

instance MonadTrans HtmlT where
  lift = HtmlT . lift

instance (Semigroup a, Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Applicative m) => Monoid (HtmlT m a) where
  mempty = HtmlT $ ReaderT \_ -> pure mempty


#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HtmlT m) where
  liftJSM' = lift . liftJSM'
#endif

instance (x ~ (), HtmlBase m) => IsString (HtmlT m x) where
  fromString = text . T.pack
    where
      -- FIXME: Duplicated code from other modules
      text :: HtmlBase m => JSString -> HtmlT m ()
      text txt = do
        textNode <- liftJSM $ jsg "document" # "createTextNode" $ [txt]
        void $ newElementRef (coerce textNode)

      newElementRef :: HtmlBase m => Element -> HtmlT m ElementRef
      newElementRef initial = do
        rootEl <- askElement
        rootFrag <- askFragment
        frag <- fmap coerce $ liftJSM $ jsg "document" # "createDocumentFragment" $ ()
        elementRef <- liftIO (newIORef initial)
        un <- lift askUnliftIO
        liftJSM $ rootFrag # "appendChild" $ initial
        let
          read = readIORef elementRef
          readFrag = pure frag
          replace newEl = do
            oldEl <- readIORef elementRef
            void $ unliftIO un $ liftJSM (rootEl # "replaceChild" $ (newEl, oldEl))
            writeIORef elementRef newEl
        pure (ElementRef read readFrag replace)

      askElement :: HtmlBase m => HtmlT m Element
      askElement =
        liftIO =<< asks (er_read . he_element)

      askFragment :: HtmlBase m => HtmlT m Fragment
      askFragment =
        liftIO =<< asks (er_fragment . he_element)
