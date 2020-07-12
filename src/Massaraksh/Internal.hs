{-# LANGUAGE TupleSections #-}
module Massaraksh.Internal where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Bool
import Data.Foldable
import Data.IORef
import Data.List
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event
import Massaraksh.Types
import qualified Control.Exception as E
import qualified Data.Sequence as Seq

newElementRef :: Element -> Html ElementRef
newElementRef elm = do
  jsCtx <- asks he_js_context
  mutateRoot (flip appendChild elm)
  let
    read = pure elm
    mutate m = runJSM (m elm) jsCtx
  pure (ElementRef read mutate)

newElementRef' :: ElementRef -> JSM (ElementRef, IO ())
newElementRef' ElementRef{..} = do
  flushedRef <- liftIO (newIORef False)
  queueRef <- liftIO (newIORef Seq.empty)
  un <- askUnliftIO
  let
    mutate m = readIORef flushedRef
      >>= bool (modifyIORef queueRef (Seq.>< Seq.singleton m))
        (er_queue_mutation m)
    flush = do
      writeIORef flushedRef True
      queue <- atomicModifyIORef' queueRef (Seq.empty,)
      er_queue_mutation \rootEl -> for_ queue ($ rootEl)
  pure (ElementRef er_read mutate, flush)

askElement :: Html Element
askElement =
  liftIO =<< asks (er_read . he_element)
{-# INLINE askElement #-}

mutateRoot :: (Element -> JSM ()) -> Html ()
mutateRoot f =
  liftIO =<< asks (($ f). er_queue_mutation . he_element)
{-# INLINE mutateRoot #-}

askMutateRoot :: Html ((Element -> JSM ()) -> IO ())
askMutateRoot = asks (er_queue_mutation . he_element)
{-# INLINE askMutateRoot #-}

localElement :: Element -> Html a -> Html a
localElement elm child = do
  elRef <- newElementRef elm
  local (\env -> env { he_element = elRef }) child
{-# INLINE localElement #-}

htmlSubscribe :: Event a -> (a -> Reactive ()) -> Html (IO ())
htmlSubscribe e k = do
  s <- asks (unSubscriber . he_subscribe)
  h <- asks he_catch_interactive
  let k' x = k x `catchSync` (liftIO . h)
  liftIO $ sync (s e k')
{-# INLINE htmlSubscribe #-}

newSubscriber :: IO (Subscriber, Subscriptions)
newSubscriber = do
  subs <- newIORef []
  let
    subscriber = Subscriber \e f -> do
      unsub <- e `subscribe` f
      unRef <- liftIO (newIORef unsub)
      liftIO $ modifyIORef subs ((:) unRef)
      pure $ liftIO $ modifyIORef subs (delete unRef)
  pure (subscriber, subs)

subscribeUpdates :: Dynamic s -> Callback s -> Html (IO ())
subscribeUpdates d f = dyn_updates d `htmlSubscribe` f
{-# INLINE subscribeUpdates #-}

forDyn :: Dynamic a -> Callback a -> Html (IO ())
forDyn dyn k = do
  liftIO (dyn_read dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k

catchSync :: (MonadCatch m, MonadThrow m) => m a -> (SomeException -> m a) -> m a
catchSync io h = io `catch` \e ->
  case E.fromException e of
    Just (E.SomeAsyncException _) -> throwM e
    Nothing                       -> h e
