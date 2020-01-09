module Massaraksh.Internal where

import Massaraksh.Types
import Massaraksh.Event
import Language.Javascript.JSaddle
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.List

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
  subscriber <- asks (sbscrPrivate . sbrefValue . hteSubscriberRef)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO . f)

subscribePublic
  :: MonadHtmlBase m
  => Event (ComponentT w s t m ())
  -> HtmlT w s t m (IO ())
subscribePublic e = do
  subscriber <- asks (sbscrPublic . sbrefValue . hteSubscriberRef)
  UnliftIO{..} <- askUnliftIO
  liftIO (subscriber e)

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
