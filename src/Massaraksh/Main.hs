{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Exception
import Control.Natural hiding ((#))
import Data.IORef
import Data.Coerce
import Language.Javascript.JSaddle
import Massaraksh.Internal
import Massaraksh.Types
import Massaraksh.DOM

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
#endif

data RunningState a = RunningState {
  rsResult :: a,
  rsEnv    :: HtmlEnv
}

attach
  :: Element -- ^ Root DOM node
  -> Html x  -- ^ Render action
  -> JSM (RunningState x)
attach rootEl render = do
  js <- askJSM
  evalRef <- liftIO $ newIORef \_ -> pure ()
  (subscriber, subscriptions) <- liftIO newSubscriber
  postHooks <- liftIO (newIORef [])
  let rootRef = ElementRef (pure rootEl) (\_ -> pure ())
  (elRef, flush) <- newElementRef' rootRef
  let env = HtmlEnv elRef subscriber postHooks js throwIO
  Liftio $ writeIORef evalRef \(Exist h) -> void (runHtml env h)
  res <- liftIO $ runHtml env render
  liftIO flush
  liftIO (readIORef postHooks >>= mapM_ (runHtml env))
  pure (RunningState res env)

attachToBody
  :: Html x -- ^ Render action
  -> JSM (RunningState x)
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
