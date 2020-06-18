module Massaraksh.Internal where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List
import Data.Coerce
import Language.Javascript.JSaddle
import Massaraksh.Event
import Massaraksh.Types
import Massaraksh.DOM

newElementRef :: HtmlBase m => Element -> HtmlT m ElementRef
newElementRef initial = do
  rootEl <- askElement
  rootFrag <- askFragment
  frag <- liftJSM createDocumentFragment
  elementRef <- liftIO (newIORef initial)
  un <- lift askUnliftIO
  liftJSM (appendChild rootFrag initial)
  let
    read = readIORef elementRef
    readFrag = pure frag
    replace newEl = do
      oldEl <- readIORef elementRef
      void $ unliftIO un $ liftJSM (replaceChild rootEl newEl oldEl)
      writeIORef elementRef newEl
  pure (ElementRef read readFrag replace)

askElement :: HtmlBase m => HtmlT m Element
askElement =
  liftIO =<< asks (er_read . he_element)
{-# INLINE askElement #-}

askFragment :: HtmlBase m => HtmlT m Fragment
askFragment =
  liftIO =<< asks (er_fragment . he_element)
{-# INLINE askFragment #-}

writeElement :: HtmlBase m => Element -> HtmlT m ()
writeElement el = do
  elRef <- asks he_element
  liftIO $ er_write elRef el
{-# INLINE writeElement #-}

withNewChild :: HtmlBase m => Element -> HtmlT m ()
withNewChild elm = do
  void (newElementRef (coerce elm))
{-# INLINE withNewChild #-}

flushFragment :: HtmlBase m => HtmlT m ()
flushFragment = do
  rootEl <- askElement
  rootFrag <- askFragment
  liftJSM $ appendChild rootEl rootFrag
{-# INLINE flushFragment #-}

localElement
  :: HtmlBase m
  => Element
  -> HtmlT m a
  -> HtmlT m a
localElement elm child = do
  elRef <- newElementRef elm
  local (\env -> env { he_element = elRef }) child
{-# INLINE localElement #-}

subscribePrivate
  :: HtmlBase m
  => Event a
  -> (a -> HtmlT m ())
  -> HtmlT m (IO ())
subscribePrivate e f = do
  subscriber <- asks (subscriberRefPrivate . he_subscriber)
  un <- askUnliftIO
  liftIO $ e `subscriber` (unliftIO un . f)

subscribePublic
  :: HtmlBase m
  => Event (HtmlT m x)
  -> HtmlT m (IO ())
subscribePublic e = do
  subscriber <- asks (subscriberRefPublic . he_subscriber)
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
  let subscriptionsRef = subscriberRefSubscriptions (he_subscriber env)
  xs <- atomicModifyIORef subscriptionsRef \xs -> ([], xs)
  for_ xs $ readIORef >=> id

subscribeUpdates
  :: HtmlBase m
  => Dyn s
  -> (s -> HtmlT m ())
  -> HtmlT m (IO ())
subscribeUpdates d f = do
  updates d `subscribePrivate` f

forDyn :: HtmlBase m => Dyn a -> (a -> HtmlT m ()) -> HtmlT m (IO ())
forDyn dyn k = do
  liftIO (readDyn dyn) >>= k
  subscribeUpdates dyn k
