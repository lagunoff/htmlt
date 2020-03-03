module Massaraksh.Internal where

import Massaraksh.Types
import Massaraksh.Event
import Massaraksh.Dynamic
import Language.Javascript.JSaddle
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Foldable
import Data.List

newElementRef :: HtmlBase m => HtmlT s m ElementRef
newElementRef = do
  rootEl <- askElement
  elementRef <- liftIO (newIORef Nothing)
  UnliftIO{..} <- lift askUnliftIO
  let
    initial   = error "Root element was accessed before it was initialized"
    relmRead  = fromMaybe initial <$> readIORef elementRef
    relmWrite = \new -> do
      readIORef elementRef >>= \old -> case (old, new) of
        (Nothing, Just newEl)    -> void $ unliftIO $ liftJSM
          (rootEl # "appendChild" $ newEl)
        (Just oldEl, Just newEl) -> void $ unliftIO $ liftJSM
          (rootEl # "replaceChild" $ (newEl, oldEl))
        (Just oldEl, Nothing)    -> void $ unliftIO $ liftJSM
          (rootEl # "removeChild" $ oldEl)
        (Nothing, Nothing)       -> pure ()
      writeIORef elementRef new
  pure ElementRef{..}

askElement :: HtmlBase m => HtmlT s m Element
askElement =
  liftIO =<< asks (relmRead . hteElement)

writeElement :: HtmlBase m => Element -> HtmlT s m ()
writeElement el = do
  ElementRef{..} <- asks hteElement
  liftIO $ relmWrite (Just el)

appendChild :: HtmlBase m => Node -> HtmlT s m ()
appendChild elm = do
  hteElement <- newElementRef
  liftIO $ relmWrite hteElement (Just elm)

localElement
  :: HtmlBase m
  => Element
  -> HtmlT s m a
  -> HtmlT s m a
localElement elm child = do
  hteElement <- newElementRef
  liftIO $ relmWrite hteElement (Just elm)
  local (\env -> env { hteElement }) child

subscribePrivate
  :: HtmlBase m
  => Event a
  -> (a -> HtmlT s m ())
  -> HtmlT s m (IO ())
subscribePrivate e f = do
  subscriber <- asks (sbscrPrivate . sbrefValue . hteSubscriber)
  UnliftIO{..} <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO . f)

subscribePublic
  :: HtmlBase m
  => Event (HtmlT s m x)
  -> HtmlT s m (IO ())
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

askModel :: HtmlBase m => HtmlT s m s
askModel = do
  read <- asks (dynRead . drefValue . hteModel)
  liftIO read

htmlFinalize :: HtmlEnv s m -> IO ()
htmlFinalize env = do
  let subscriptionsRef = sbrefSubscriptions (hteSubscriber env)
  xs <- atomicModifyIORef subscriptionsRef \xs -> ([], xs)
  for_ xs $ readIORef >=> id

subscribeUpdates
  :: HtmlBase m
  => (Update s -> HtmlT s m ())
  -> HtmlT s m (IO ())
subscribeUpdates f = do
  hte <- ask
  let updates = dynUpdates . drefValue . hteModel $ hte
  updates `subscribePrivate` f

