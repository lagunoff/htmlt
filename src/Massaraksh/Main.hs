{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Natural
import Data.IORef
import Language.Javascript.JSaddle
import Massaraksh.Dynamic
import Massaraksh.Internal
import Massaraksh.Types

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

data RunningState s m a = RunningState
  { rsResult :: a
  , rsEnv    :: HtmlEnv s m
  , rsEval   :: HtmlT s m ~> IO
  }

attach
  :: forall m s x
   . Monad m
  => Element     -- ^ Root DOM node
  -> (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> s           -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> JSM (RunningState s m x)
attach rootEl runM init render = do
  evalRef <- liftIO $ newIORef \_ -> pure ()
  hteModel <- liftIO (newDynamicRef init)
  hteSubscriber <- liftIO $ newSubscriberRef \e ->
    readIORef evalRef >>= ($ e)
  let
    rsEval :: forall x. HtmlT s m x -> IO x
    rsEval = runM . flip runReaderT rsEnv . runHtmlT
    hteElement = ElementRef (pure rootEl) \_ -> pure ()
    rsEnv = HtmlEnv{..}
  liftIO $ writeIORef evalRef \(Exist h) -> void (rsEval h)
  rsResult <- liftIO $ rsEval render
  pure RunningState{..}

attachToBody
  :: Monad m
  => (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> s           -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> JSM (RunningState s m x)
attachToBody runM init render = do
  rootEl <- jsg "document" ! "body"
  attach rootEl runM init render

attachSimple
  :: forall s x
   . Element       -- ^ Root DOM node
  -> s             -- ^ Initial state
  -> HtmlT s JSM x -- ^ Render action
  -> JSM (RunningState s JSM x)
attachSimple rootEl init render = do
  UnliftIO{..} <- askUnliftIO
  attach rootEl unliftIO init render

attachToBodySimple
  :: s             -- ^ Initial state
  -> HtmlT s JSM x -- ^ Render action
  -> JSM (RunningState s JSM x)
attachToBodySimple init render = do
  UnliftIO{..} <- askUnliftIO
  attachToBody unliftIO init render

attachIO
  :: forall m s x
   . Monad m
  => Element     -- ^ Root DOM node
  -> (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> s           -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> IO ()
attachIO rootEl runM init render =
  withJSM $ attach rootEl runM init render

attachToBodyIO
  :: Monad m
  => (m ~> IO)   -- ^ Evaluate application effects in @m@
  -> s           -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> IO ()
attachToBodyIO runM init render =
  withJSM $ attachToBody runM init render

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
