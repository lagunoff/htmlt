module HtmlT.Main where

import Control.Exception
import Control.Monad.Reader
import Data.IORef
import HtmlT.DOM
import HtmlT.Internal
import HtmlT.Types
import HtmlT.Event
import GHC.Generics
import qualified HtmlT.HashMap as H

data InitOpts = InitOpts
  { initopts_finalizers :: Finalizers
  , initopts_subscriptions :: Subscriptions
  , initopts_root_element :: Node
  } deriving Generic

attachOpts :: InitOpts -> HtmlIO a -> IO (a, HtmlEnv)
attachOpts InitOpts{..} render = do
  postHooks <- liftIO (newIORef [])
  let rootRef = NodeRef (pure initopts_root_element) ($ initopts_root_element)
  (rootRef', flush) <- liftIO (deferMutations rootRef)
  let env = HtmlEnv rootRef' initopts_finalizers initopts_subscriptions postHooks throwIO
  res <- liftIO $ runHtmlT env render
  liftIO flush
  liftIO (readIORef postHooks >>= sequence_)
  onBeforeUnload do
    fins <- readIORef (unFinalizers initopts_finalizers)
    sequence_ fins
  pure (res, env)

attach :: Node -> HtmlIO a -> IO (a, HtmlEnv)
attach rootEl render = do
  fins <- liftIO $ Finalizers <$> newIORef []
  subs <- liftIO $ Subscriptions <$> H.new
  attachOpts (InitOpts fins subs rootEl) render

attachToBody :: HtmlIO a -> IO (a, HtmlEnv)
attachToBody h = getBody >>= (`attach` h)

detach :: HtmlEnv -> IO ()
detach = error "Unimplemented"
