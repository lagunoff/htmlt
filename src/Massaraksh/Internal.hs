{-# LANGUAGE TupleSections #-}
module Massaraksh.Internal where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Bool
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
import Data.Text as T
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event
import Massaraksh.Types
import qualified Control.Exception as E
import qualified Data.Sequence as Seq

newRootRef :: Bool -> Node -> JSM RootRef
newRootRef adopting n = do
  js <- askJSM
  acRef <- liftIO (newIORef 0)
  chLen <- childLength n
  let
    mutate m = runJSM (m n) js
    dontAdopt = pure Nothing
    doAdopt = do
      x <- atomicModifyIORef' acRef \x -> (x + 1, x + 1)
      bool (Just <$> (runJSM (getChildNode n x) js)) (pure Nothing)
        $ x >= chLen
    adopt = bool dontAdopt doAdopt adopting
  pure $ RootRef n mutate adopt

deferMutations :: RootRef -> IO (RootRef, IO ())
deferMutations rf@RootRef{..} = do
  doneRef <- newIORef False
  queueRef <- newIORef Seq.empty
  let
    mutate m = readIORef doneRef
      >>= bool (modifyIORef queueRef (Seq.>< Seq.singleton m))
        (rrfMutate m)
    commit = do
      writeIORef doneRef True
      queue <- atomicModifyIORef' queueRef (Seq.empty,)
      rrfMutate \rootEl -> for_ queue ($ rootEl)
  pure (rf {rrfMutate = mutate}, commit)

askElement :: Html Node
askElement = asks (rrfRoot . htnvRootRef)
{-# INLINE askElement #-}

mutateRoot :: (Node -> JSM ()) -> Html ()
mutateRoot f = liftIO =<< asks (($ f). rrfMutate . htnvRootRef)
{-# INLINE mutateRoot #-}

askMutateRoot :: Html ((Node -> JSM ()) -> IO ())
askMutateRoot = asks (rrfMutate . htnvRootRef)
{-# INLINE askMutateRoot #-}

adoptElement :: JSM Node -> Html a -> Html (a, Node)
adoptElement create child = do
  rf <- asks htnvRootRef
  mayEl <- liftIO (rrfAdopt rf)
  el <- maybe (liftJSM create) pure mayEl
  newRf <- liftJSM (newRootRef True el)
  a <- local (\e -> e {htnvRootRef = newRf}) child
  when (isNothing mayEl)
    $ liftIO $ rrfMutate rf (flip appendChild el)
  pure (a, el)

adoptText :: Text -> Html ()
adoptText t = do
  rf <- asks htnvRootRef
  mayEl <- liftIO (rrfAdopt rf)
  when (isNothing mayEl) do
    tn <- liftJSM (createTextNode t)
    liftIO $ rrfMutate rf (flip appendChild tn)

adoptDynText :: Dynamic Text -> Html ()
adoptDynText d = do
  rf <- asks htnvRootRef
  js <- askJSM
  mayEl <- liftIO (rrfAdopt rf)
  t <- liftIO (dnRead d)
  el <- maybe (liftJSM (createTextNode t)) pure mayEl
  subscribeUpdates d (liftIO . flip runJSM js . setTextValue el)
  when (isNothing mayEl) do
    liftIO $ rrfMutate rf (flip appendChild el)

htmlSubscribe :: Event a -> Callback a -> Html (IO ())
htmlSubscribe e k = do
  s <- asks (unSubscriber . htnvSubscribe)
  h <- asks htnvCatchInteractive
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
subscribeUpdates d f = dnUpdates d `htmlSubscribe` f
{-# INLINE subscribeUpdates #-}

forDyn :: Dynamic a -> Callback a -> Html (IO ())
forDyn dyn k = do
  liftIO (dnRead dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k

catchSync :: (MonadCatch m, MonadThrow m) => m a -> (SomeException -> m a) -> m a
catchSync io h = io `catch` \e ->
  case E.fromException e of
    Just (E.SomeAsyncException _) -> throwM e
    Nothing                       -> h e
