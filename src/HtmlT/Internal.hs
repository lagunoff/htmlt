module HtmlT.Internal where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Bool
import Data.Foldable
import Data.IORef
import Data.List
import GHC.Generics
import Language.Javascript.JSaddle
import qualified Control.Exception as E
import qualified Data.Sequence as Seq

import HtmlT.DOM
import HtmlT.Event
import HtmlT.Types

data ElemEnv a = ElemEnv
  { ee_htmlEnv :: HtmlEnv
  , ee_Ref :: DynRef a
  , ee_modifier :: Modifier a
  }
  deriving stock Generic

newNodeRef :: Node -> HtmlT NodeRef
newNodeRef el = do
  js <- asks he_js_context
  mutateRoot (`appendChild` el)
  let
    read = pure el
    mutate m = runJSM (m el) js
  pure (NodeRef read mutate)

deferMutations :: NodeRef -> IO (NodeRef, IO ())
deferMutations NodeRef{..} = do
  flushedRef <- newIORef False
  queueRef <- newIORef Seq.empty
  let
    mutate m = readIORef flushedRef
      >>= bool (modifyIORef' queueRef (Seq.>< Seq.singleton m))
        (nr_mutate m)
    flush = do
      writeIORef flushedRef True
      queue <- atomicModifyIORef' queueRef (Seq.empty,)
      nr_mutate \rootEl -> for_ queue ($ rootEl)
  pure (NodeRef nr_read mutate, flush)

askRootNode :: HtmlT Node
askRootNode = liftIO =<< asks (nr_read . he_current_root)

mutateRoot :: (Node -> JSM ()) -> HtmlT ()
mutateRoot f = liftIO =<< asks (($ f). nr_mutate . he_current_root)

askMutateRoot :: HtmlT ((Node -> JSM ()) -> IO ())
askMutateRoot = asks (nr_mutate . he_current_root)

withRootNode :: Node -> HtmlT a -> HtmlT a
withRootNode rootEl child = do
  rootRef <- newNodeRef rootEl
  local (\env -> env { he_current_root = rootRef }) child

catchSync :: (MonadCatch m, MonadThrow m) => m a -> (SomeException -> m a) -> m a
catchSync io h = io `catch` \e -> case E.fromException e of
  Just (E.SomeAsyncException _) -> throwM e
  Nothing                       -> h e

addFinalizer :: (MonadIO m, MonadFinalize m) => IO () -> m ()
addFinalizer fin = do
  fins <- askFinalizers
  finRef <- liftIO $ newIORef fin
  liftIO $ modifyIORef (unFinalizers fins) (fin:)
