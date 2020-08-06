{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Exception
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Coerce
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

attach :: Node -> Html a -> JSM (a, HtmlEnv)
attach rootEl render = do
  js <- askJSM
  evalRef <- liftIO $ newIORef \_ -> pure ()
  (subscriber, subscriptions) <- liftIO newSubscriber
  postHooks <- liftIO (newIORef [])
  let rootRef = ElementRef (pure rootEl) (flip runJSM js . ($ rootEl))
  (elRef, flush) <- newElementRef' rootRef
  let env = HtmlEnv elRef subscriber postHooks js throwIO
  liftIO $ writeIORef evalRef \(Exist h) -> void (runHtml env h)
  res <- liftIO $ runHtml env render
  liftIO flush
  liftIO (readIORef postHooks >>= mapM_ (runHtml env))
  pure (res, env)

attachToBody :: Html a -> JSM (a, HtmlEnv)
attachToBody render = do
  rootEl <- fmap coerce $ jsg "document" ! "body"
  attach rootEl render

withJSM :: JSM x -> IO ()
#ifdef ghcjs_HOST_OS
withJSM jsm = do _ <- jsm; pure ()
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
