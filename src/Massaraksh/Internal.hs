{-# LANGUAGE TupleSections #-}
module Massaraksh.Internal where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Bool
import Data.ByteString.Builder
import Data.Foldable
import Data.HashTable.IO as HT
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Text as T
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Event
import Massaraksh.Types
import Debug.Trace
import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import qualified Control.Exception as E
import qualified Data.Sequence as Seq

renderNode :: Node -> IO Builder
renderNode (SsrText ref) =
  B.fromHtmlEscapedText <$> readIORef ref
renderNode (SsrElement ns t attHt chRef) = do
  ch <- readIORef chRef
  c <- fold <$> mapM renderNode ch
  let renderAtt (k,v) = B.fromText k <> B.fromString "=\"" <> B.fromHtmlEscapedText v <> B.fromString "\""
  att0 <- HT.toList attHt
  let att1 = fold $ L.intersperse (B.fromChar ' ') $ fmap renderAtt att0
  let att2 = bool (B.fromChar ' ' <> att1) att1 $ att0 == mempty
  let begin = B.fromChar '<' <> B.fromHtmlEscapedText t <> att2 <> B.fromChar '>'
  let end = B.fromString "</" <> B.fromHtmlEscapedText t <> B.fromChar '>'
  pure $ begin <> c <> end

newRootRef :: Node -> JSM RootRef
newRootRef n = do
  js <- askJSM
  acRef <- liftIO (newIORef 0)
  let
    mutate m = runJSM (m n) js
    nextChild = liftIO (atomicModifyIORef' acRef \x -> (x + 1, x))
      >>= getChildNode n >>= \case
        Nothing -> pure Nothing
        Just e  -> Just . (e,) <$> isText e
    appendText t = nextChild >>= \case
      Nothing -> createTextNode n t
        >>= \e -> e <$ liftIO (mutate (flip appendChild e))
      Just (e, True) -> pure e
      Just (e, False) -> pure e -- TODO: replace Element with Text
    appendEl create = nextChild >>= \case
      Nothing -> create n
        >>= \e -> e <$ liftIO (mutate (flip appendChild e))
      Just (e, False) -> pure e
      Just (e, True) -> appendEl create
  pure $ RootRef n mutate acRef appendText appendEl

deferMutations :: RootRef -> IO (RootRef, IO ())
deferMutations rf@RootRef{..} = do
  doneRef <- newIORef False
  queueRef <- newIORef Seq.empty
  let
    n = rrfRoot
    mutate m = readIORef doneRef
      >>= bool (modifyIORef queueRef (Seq.>< Seq.singleton m))
        (rrfMutate m)
    commit = do
      writeIORef doneRef True
      queue <- atomicModifyIORef' queueRef (Seq.empty,)
      rrfMutate \rootEl -> for_ queue ($ rootEl)
    nextChild = liftIO (atomicModifyIORef' rrfAppendCount \x -> (x + 1, x))
      >>= getChildNode n >>= \case
        Nothing -> pure Nothing
        Just e  -> Just . (e,) <$> isText e
    appendText t = nextChild >>= \case
      Nothing -> createTextNode n t
        >>= \e -> e <$ liftIO (mutate (flip appendChild e))
      Just (e, True) -> pure e
      Just (e, False) -> pure e -- TODO: replace Element with Text
    appendEl create = nextChild >>= \case
      Nothing -> create n
        >>= \e -> e <$ liftIO (mutate (flip appendChild e))
      Just (e, False) -> pure e
      Just (e, True) -> appendEl create
  pure (rf {rrfMutate = mutate, rrfAppendText = appendText, rrfAppendElement = appendEl}, commit)

askElement :: Html Node
askElement = asks (rrfRoot . htnvRootRef)
{-# INLINE askElement #-}

mutateRoot :: (Node -> JSM ()) -> Html ()
mutateRoot f = liftIO =<< asks (($ f). rrfMutate . htnvRootRef)
{-# INLINE mutateRoot #-}

askMutateRoot :: Html ((Node -> JSM ()) -> IO ())
askMutateRoot = asks (rrfMutate . htnvRootRef)
{-# INLINE askMutateRoot #-}

adoptElement :: (Node -> JSM Node) -> Html a -> Html (a, Node)
adoptElement create child = do
  rf <- asks htnvRootRef
  el <- liftJSM (rrfAppendElement rf create)
  newRf <- liftJSM (newRootRef el)
  a <- local (\e -> e {htnvRootRef = newRf}) child
  pure (a, el)

adoptText :: Text -> Html ()
adoptText t = do
  rf <- asks htnvRootRef
  void $ liftJSM (rrfAppendText rf t)

adoptDynText :: Dynamic Text -> Html ()
adoptDynText d = do
  js <- askJSM
  rf <- asks htnvRootRef
  t <- liftIO (dnRead d)
  el <- liftJSM (rrfAppendText rf t)
  void $ subscribeUpdates d (liftIO . flip runJSM js . setTextValue el)

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
      pure $ liftIO $ modifyIORef subs (L.delete unRef)
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
