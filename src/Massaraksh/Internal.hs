module Massaraksh.Internal where

import Massaraksh.Types
import Massaraksh.Event
import Language.Javascript.JSaddle
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.List

newElementRef :: HtmlBase m => HtmlT s t m ElementRef
newElementRef = do
  rootEl <- readElement
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
  pure ElementRef{..}

readElement :: HtmlBase m => HtmlT s t m Element
readElement =
  asks (relmRead . hteElement) >>= liftIO

writeElement :: HtmlBase m => Element -> HtmlT s t m ()
writeElement el = do
  ElementRef{..} <- asks hteElement
  liftIO (relmWrite el)

appendChild :: HtmlBase m => Node -> HtmlT s t m ()
appendChild elm = do
  hteElement@ElementRef{..} <- newElementRef
  liftIO (relmWrite elm)

localElement
  :: HtmlBase m
  => Element
  -> HtmlT s t m a
  -> HtmlT s t m a
localElement elm child = do
  hteElement@ElementRef{..} <- newElementRef
  liftIO (relmWrite elm)
  local (\env -> env { hteElement }) child

subscribePrivate
  :: HtmlBase m
  => Event a
  -> (a -> HtmlT s t m ())
  -> HtmlT s t m (IO ())
subscribePrivate e f = do
  subscriber <- asks (sbscrPrivate . sbrefValue . hteSubscriber)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO . f)

subscribePublic
  :: HtmlBase m
  => Event (HtmlT s t m x)
  -> HtmlT s t m (IO ())
subscribePublic e = do
  subscriber <- asks (sbscrPublic . sbrefValue . hteSubscriber)
  UnliftIO{..} <- askUnliftIO
  liftIO (subscriber $ Exist <$> e)

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
