module Massaraksh.Internal where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
import Language.Javascript.JSaddle
import Massaraksh.Event
import Massaraksh.Types

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

askElement :: HtmlBase m => HtmlT m Element
askElement =
  liftIO =<< asks (elementRefRead . htmlEnvElement)

writeElement :: HtmlBase m => Element -> HtmlT m ()
writeElement el = do
  elRef <- asks htmlEnvElement
  liftIO $ elementRefWrite elRef (Just el)

appendChild :: HtmlBase m => Node -> HtmlT m ()
appendChild elm = do
  elRef <- newElementRef
  liftIO $ elementRefWrite elRef (Just elm)

localElement
  :: HtmlBase m
  => Element
  -> HtmlT m a
  -> HtmlT m a
localElement elm child = do
  elRef <- newElementRef
  liftIO $ elementRefWrite elRef (Just elm)
  local (\env -> env { htmlEnvElement = elRef }) child

subscribePrivate
  :: HtmlBase m
  => Event a
  -> (a -> HtmlT m ())
  -> HtmlT m (IO ())
subscribePrivate e f = do
  subscriber <- asks (subscriberRefPrivate . htmlEnvSubscriber)
  un <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO un . f)

subscribePublic
  :: HtmlBase m
  => Event (HtmlT m x)
  -> HtmlT m (IO ())
subscribePublic e = do
  subscriber <- asks (subscriberRefPublic . htmlEnvSubscriber)
  liftIO (subscriber $ Exist <$> e)

newSubscriberRef :: (a -> IO ()) -> IO (SubscriberRef a)
newSubscriberRef k = do
  subs <- newIORef []
  let
    private :: forall x. Event x -> (x -> IO ()) -> IO (IO ())
    private = \e f -> do
      unsub <- e `subscribe` f
      unRef <- newIORef unsub
      modifyIORef subs ((:) unRef)
      pure $ modifyIORef subs (delete unRef)
    public = \e -> do
      unsub <- e `subscribe` k
      unRef <- newIORef unsub
      modifyIORef subs ((:) unRef)
      pure $ modifyIORef subs (delete unRef)
  pure (SubscriberRef private public subs)

htmlFinalize :: HtmlEnv m -> IO ()
htmlFinalize env = do
  let subscriptionsRef = subscriberRefSubscriptions (htmlEnvSubscriber env)
  xs <- atomicModifyIORef subscriptionsRef \xs -> ([], xs)
  for_ xs $ readIORef >=> id

subscribeUpdates
  :: HtmlBase m
  => Dyn s
  -> (s -> HtmlT m ())
  -> HtmlT m (IO ())
subscribeUpdates d f = do
  updates d `subscribePrivate` f

