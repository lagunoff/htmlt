{-# LANGUAGE CPP #-}
module HtmlT.Main where

import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Data.Text
import Language.Javascript.JSaddle

import HtmlT.DOM
import HtmlT.Internal
import HtmlT.Types
import HtmlT.Event
import qualified HtmlT.HashMap as H

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
#endif

data InitOpts = InitOpts
  { initopts_finalizers :: Finalizers
  , initopts_subscriptions :: Subscriptions
  , initopts_root_element :: Node
  }

attachOpts :: InitOpts -> HtmlT a -> JSM (a, HtmlEnv)
attachOpts InitOpts{..} render = do
  js <- askJSM
  postHooks <- liftIO (newIORef [])
  let rootRef = NodeRef (pure initopts_root_element) (flip runJSM js . ($ initopts_root_element))
  (rootRef', flush) <- liftIO (deferMutations rootRef)
  let env = HtmlEnv rootRef' initopts_finalizers initopts_subscriptions postHooks js throwIO
  res <- liftIO $ runHtmlT env render
  liftIO flush
  liftIO (readIORef postHooks >>= mapM_ (runHtmlT env))
  onBeforeUnload do
    fins <- readIORef (unFinalizers initopts_finalizers)
    sequence_ fins
  pure (res, env)

attach :: Node -> HtmlT a -> JSM (a, HtmlEnv)
attach rootEl render = do
  fins <- liftIO $ Finalizers <$> newIORef []
  subs <- liftIO $ Subscriptions <$> H.new
  attachOpts (InitOpts fins subs rootEl) render

attachToBody :: HtmlT a -> JSM (a, HtmlEnv)
attachToBody h = getCurrentBody >>= (`attach` h)

withJSM :: JSM x -> IO ()
#ifdef ghcjs_HOST_OS
withJSM = void
#else
withJSM jsm = do
  envPort <- either (const Nothing) Just <$>
    try @SomeException (read <$> getEnv "PORT")
  progName <- getProgName
  let
    Just port = envPort <|> Just 8080
    runWarp = if progName == "<interactive>" then Warp.debug else Warp.run
    message = "Running jsaddle-warp application on http://localhost:"
      <> show port <> "/"
  putStrLn message
  runWarp port (void jsm)
#endif
