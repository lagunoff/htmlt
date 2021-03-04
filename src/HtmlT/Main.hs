{-# LANGUAGE CPP #-}
module HtmlT.Main where

import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Language.Javascript.JSaddle

import HtmlT.DOM
import HtmlT.Internal
import HtmlT.Types

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
#endif

attach :: Node -> HtmlT a -> JSM (a, HtmlEnv)
attach rootEl render = do
  js <- askJSM
  subscriptions <- liftIO (newIORef [])
  postHooks <- liftIO (newIORef [])
  let rootRef = NodeRef (pure rootEl) (flip runJSM js . ($ rootEl))
  (elmRef, flush) <- liftIO (deferMutations rootRef)
  let env = HtmlEnv elmRef subscriptions postHooks js throwIO
  res <- liftIO $ runHtmlT env render
  liftIO flush
  liftIO (readIORef postHooks >>= mapM_ (runHtmlT env))
  pure (res, env)

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
