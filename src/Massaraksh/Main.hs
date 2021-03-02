{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Language.Javascript.JSaddle
import Massaraksh.DOM
import Massaraksh.Internal
import Massaraksh.Types

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
  let rootRef = ElementRef (pure rootEl) (flip runJSM js . ($ rootEl))
  (elRef, flush) <- liftIO (deferMutations rootRef)
  let env = HtmlEnv elRef subscriptions postHooks js throwIO
  res <- liftIO $ runHtmlT env render
  liftIO flush
  liftIO (readIORef postHooks >>= mapM_ (runHtmlT env))
  pure (res, env)

attachToBody :: HtmlT a -> JSM (a, HtmlEnv)
attachToBody h = getCurrentBody >>= (`attach` h)

portal :: Node -> HtmlT a -> HtmlT a
portal rootEl render = do
  js <- askJSM
  env <- ask
  let rootRef = ElementRef (pure rootEl) (flip runJSM js . ($ rootEl))
  local (\e -> e {htmlEnv_element = rootRef}) render

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
