{-# LANGUAGE TupleSections #-}
module Massaraksh.Internal where

import Control.Monad.Catch
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

newElementRef :: Node -> Html ElementRef
newElementRef elm = do
  jsCtx <- asks htenvJsContext
  mutateRoot (flip appendChild elm)
  let
    read = pure elm
    mutate m = runJSM (m elm) jsCtx
  pure (ElementRef read mutate)

deferMutations :: ElementRef -> JSM (ElementRef, IO ())
deferMutations ElementRef{..} = do
  flushedRef <- liftIO (newIORef False)
  queueRef <- liftIO (newIORef Seq.empty)
  let
    mutate m = readIORef flushedRef
      >>= bool (modifyIORef queueRef (Seq.>< Seq.singleton m))
        (elrfQueueMutation m)
    flush = do
      writeIORef flushedRef True
      queue <- atomicModifyIORef' queueRef (Seq.empty,)
      elrfQueueMutation \rootEl -> for_ queue ($ rootEl)
  pure (ElementRef elrfRead mutate, flush)

askElement :: Html Node
askElement = liftIO =<< asks (elrfRead . htenvElement)
{-# INLINE askElement #-}

mutateRoot :: (Node -> JSM ()) -> Html ()
mutateRoot f = liftIO =<< asks (($ f). elrfQueueMutation . htenvElement)
{-# INLINE mutateRoot #-}

askMutateRoot :: Html ((Node -> JSM ()) -> IO ())
askMutateRoot = asks (elrfQueueMutation . htenvElement)
{-# INLINE askMutateRoot #-}

localElement :: Node -> Html a -> Html a
localElement elm child = do
  elRef <- newElementRef elm
  local (\env -> env { htenvElement = elRef }) child
{-# INLINE localElement #-}

htmlSubscribe :: Event a -> (a -> Reactive ()) -> Html (IO ())
htmlSubscribe e k = do
  s <- asks (unSubscriber . htenvSubscriber)
  h <- asks htenvCatchInteractive
  let k' x = k x `catchSync` (liftIO . h)
  liftIO $ sync (s e k')
{-# INLINE htmlSubscribe #-}

newSubscriber :: IO (Subscriber, Subscriptions)
newSubscriber = do
  subscriptions <- newIORef []
  let
    subscriber = Subscriber \e f -> do
      unsub <- e `subscribe` f
      unRef <- liftIO (newIORef unsub)
      liftIO $ modifyIORef subscriptions ((:) unRef)
      pure $ liftIO $ modifyIORef subscriptions (delete unRef)
  pure (subscriber, subscriptions)

subscribeUpdates :: Dynamic s -> Callback s -> Html (IO ())
subscribeUpdates d f = dnUpdates d `htmlSubscribe` f
{-# INLINE subscribeUpdates #-}

forDyn :: Dynamic a -> Callback a -> Html (IO ())
forDyn dyn k = do
  liftIO (dnRead dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k

catchSync :: (MonadCatch m, MonadThrow m) => m a -> (SomeException -> m a) -> m a
catchSync io h = io `catch` \e -> case E.fromException e of
  Just (E.SomeAsyncException _) -> throwM e
  Nothing                       -> h e
