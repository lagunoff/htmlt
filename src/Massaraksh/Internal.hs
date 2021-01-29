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

htmlSubscribe :: Event a -> Callback a -> Html (IO ())
htmlSubscribe e k = do
  sRef <- asks htenvSubscriptions
  handle <- asks htenvCatchInteractive
  let k' x = k x `catchSync` (liftIO . handle)
  liftIO do
    unsub <- e `subscribe` k'
    unRef <- newIORef unsub
    modifyIORef sRef ((:) unRef)
    pure $ modifyIORef sRef (delete unRef)

subscribeUpdates :: Dynamic s -> Callback s -> Html (IO ())
subscribeUpdates d f = dynamic_updates d `htmlSubscribe` f
{-# INLINE subscribeUpdates #-}

forDyn :: Dynamic a -> Callback a -> Html (IO ())
forDyn dyn k = do
  liftIO (dynamic_read dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k

catchSync :: (MonadCatch m, MonadThrow m) => m a -> (SomeException -> m a) -> m a
catchSync io h = io `catch` \e -> case E.fromException e of
  Just (E.SomeAsyncException _) -> throwM e
  Nothing                       -> h e

addFinalizer :: IO () -> Html ()
addFinalizer fin = do
  subs <- asks htenvSubscriptions
  finRef <- liftIO $ newIORef fin
  liftIO $ modifyIORef subs (finRef :)
