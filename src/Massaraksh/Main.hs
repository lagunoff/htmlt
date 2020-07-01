{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
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
import Control.Exception
#endif

data RunningState m a = RunningState {
  rsResult :: a,
  rsEnv    :: HtmlEnv m,
  rsEval   :: HtmlT m ~> IO
}

attach
  :: forall m x
   . Monad m
  => Element     -- ^ Root DOM node
  -> (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> HtmlT m x -- ^ Render action
  -> JSM (RunningState m x)
attach rootEl runM render = do
  evalRef <- liftIO $ newIORef \_ -> pure ()
  (subscriber, subscriptions) <- liftIO newSubscriber
  frag <- fmap coerce $ jsg "document" # "createDocumentFragment" $ ()
  postHooks <- liftIO $ newIORef []
  let
    eval :: forall x. HtmlT m x -> IO x
    eval  = runM . runHtmlT env
    {-# INLINE eval #-}
    elRef = ElementRef (pure rootEl) (pure frag) \_ -> pure ()
    env   = HtmlEnv elRef subscriber postHooks
  liftIO $ writeIORef evalRef \(Exist h) -> void (eval h)
  res <- liftIO $ eval render
  rootEl # "appendChild" $ frag
  liftIO (readIORef postHooks >>= mapM_ eval)
  pure (RunningState res env eval)

attachToBody
  :: Monad m
  => (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> HtmlT m x -- ^ Render action
  -> JSM (RunningState m x)
attachToBody runM render = do
  rootEl <- fmap coerce $ jsg "document" ! "body"
  attach rootEl runM render

attachSimple
  :: forall x
   . Element       -- ^ Root DOM node
  -> HtmlT JSM x -- ^ Render action
  -> JSM (RunningState JSM x)
attachSimple rootEl render = do
  un <- askUnliftIO
  attach rootEl (unliftIO un) render

attachToBodySimple
  :: HtmlT JSM x -- ^ Render action
  -> JSM (RunningState JSM x)
attachToBodySimple render = do
  un <- askUnliftIO
  attachToBody (unliftIO un) render

attachIO
  :: forall m x
   . Monad m
  => Element     -- ^ Root DOM node
  -> (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> HtmlT m x -- ^ Render action
  -> IO ()
attachIO rootEl runM render =
  withJSM $ attach rootEl runM render

attachToBodyIO
  :: Monad m
  => (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> HtmlT m x -- ^ Render action
  -> IO ()
attachToBodyIO runM render =
  withJSM $ attachToBody runM render

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
