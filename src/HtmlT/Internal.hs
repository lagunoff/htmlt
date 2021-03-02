module HtmlT.Internal where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Bool
import Data.Foldable
import Data.IORef
import Data.List
import Language.Javascript.JSaddle
import qualified Control.Exception as E
import qualified Data.Sequence as Seq

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Types

newElementRef :: Node -> HtmlT ElementRef
newElementRef elm = do
  jsCtx <- asks htmlEnv_jsContext
  mutateRoot (flip appendChild elm)
  let
    read = pure elm
    mutate m = runJSM (m elm) jsCtx
  pure (ElementRef read mutate)

deferMutations :: ElementRef -> IO (ElementRef, IO ())
deferMutations ElementRef{..} = do
  flushedRef <- newIORef False
  queueRef <- newIORef Seq.empty
  let
    mutate m = readIORef flushedRef
      >>= bool (modifyIORef queueRef (Seq.>< Seq.singleton m))
        (elementRef_mutate m)
    flush = do
      writeIORef flushedRef True
      queue <- atomicModifyIORef' queueRef (Seq.empty,)
      elementRef_mutate \rootEl -> for_ queue ($ rootEl)
  pure (ElementRef elementRef_read mutate, flush)

askElement :: HtmlT Node
askElement = liftIO =<< asks (elementRef_read . htmlEnv_element)

mutateRoot :: (Node -> JSM ()) -> HtmlT ()
mutateRoot f = liftIO =<< asks (($ f). elementRef_mutate . htmlEnv_element)

askMutateRoot :: HtmlT ((Node -> JSM ()) -> IO ())
askMutateRoot = asks (elementRef_mutate . htmlEnv_element)

withElement :: Node -> HtmlT a -> HtmlT a
withElement  elm child = do
  elRef <- newElementRef elm
  local (\env -> env { htmlEnv_element = elRef }) child

htmlSubscribe :: Event a -> Callback a -> HtmlT (IO ())
htmlSubscribe e k = do
  finsRef <- asks htmlEnv_finalizers
  handle <- asks htmlEnv_catchInteractive
  let k' x = k x `catchSync` (liftIO . handle)
  liftIO do
    unsub <- e `subscribe` k'
    unsubRef <- newIORef unsub
    modifyIORef finsRef ((:) unsubRef)
    pure $ modifyIORef finsRef (delete unsubRef) *> unsub

subscribeUpdates :: Dynamic s -> Callback s -> HtmlT (IO ())
subscribeUpdates d f = dynamic_updates d `htmlSubscribe` f
{-# INLINE subscribeUpdates #-}

forDyn :: Dynamic a -> Callback a -> HtmlT (IO ())
forDyn dyn k = do
  liftIO (dynamic_read dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k

catchSync :: (MonadCatch m, MonadThrow m) => m a -> (SomeException -> m a) -> m a
catchSync io h = io `catch` \e -> case E.fromException e of
  Just (E.SomeAsyncException _) -> throwM e
  Nothing                       -> h e

addFinalizer :: IO () -> HtmlT ()
addFinalizer fin = do
  subs <- asks htmlEnv_finalizers
  finRef <- liftIO $ newIORef fin
  liftIO $ modifyIORef subs (finRef :)
