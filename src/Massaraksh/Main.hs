{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Natural
import Language.Javascript.JSaddle
import Massaraksh.Dynamic
import Massaraksh.Types
import Massaraksh.Internal

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

attach
  :: forall m s x
   . Monad m
  => Element       -- ^ Root DOM node
  -> (m ~> IO)     -- ^ Evaluate application effects in @m@
  -> s             -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> JSM x
attach rootEl runM init render = mdo
  let
    runHtml :: forall a. HtmlT s m a -> IO a
    runHtml = runM . flip runReaderT htmlEnv . runHtmlT
    hteElement = ElementRef (pure rootEl) (\_ -> pure ())
    htmlEnv = HtmlEnv{..}
  hteModel <- liftIO (newDynamicRef init)
  hteSubscriber <- liftIO (newSubscriberRef $ \(Exist h) -> void (runHtml h))
  liftIO $ runHtml render

attachToBody
  :: Monad m
  => (m ~> IO)     -- ^ Evaluate application effects in @m@
  -> s             -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> JSM x
attachToBody runM init render = do
  rootEl <- jsg "document" ! "body"
  attach rootEl runM init render

attachSimple
  :: forall s x
   . Element         -- ^ Root DOM node
  -> s               -- ^ Initial state
  -> HtmlT s JSM x -- ^ Render action
  -> JSM x
attachSimple rootEl init render = do
  UnliftIO{..} <- askUnliftIO
  attach rootEl unliftIO init render

attachToBodySimple
  :: s               -- ^ Initial state
  -> HtmlT s JSM x -- ^ Render action
  -> JSM x
attachToBodySimple init render = do
  UnliftIO{..} <- askUnliftIO
  attachToBody unliftIO init render

attachIO
  :: forall m s x
   . Monad m
  => Element       -- ^ Root DOM node
  -> (m ~> IO)     -- ^ Evaluate application effects in @m@
  -> s             -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> IO ()
attachIO rootEl runM init render =
  withJSM $ attach rootEl runM init render

attachToBodyIO
  :: Monad m
  => (m ~> IO)     -- ^ Evaluate application effects in @m@
  -> s             -- ^ Initial state
  -> HtmlT s m x -- ^ Render action
  -> IO ()
attachToBodyIO runM init render =
  withJSM $ attachToBody runM init render

withJSM :: JSM x -> IO ()
#ifdef ghcjs_HOST_OS
withJSM = id
#else
withJSM jsm = do
  envPort <- either (const Nothing) Just <$> try @SomeException (read <$> getEnv "PORT")
  progName <- getProgName
  let Just port = envPort <|> Just 8080
  let runWarp = if progName == "<interactive>" then Warp.debug else Warp.run
  putStrLn $ "Running jsaddle-warp application on http://localhost:" <> show port <> "/"
  runWarp port (void jsm)
#endif
