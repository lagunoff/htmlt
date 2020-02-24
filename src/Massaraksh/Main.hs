{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Natural
import Language.Javascript.JSaddle
import Massaraksh.Dynamic
import Massaraksh.Types
import Massaraksh.Internal
import Data.IORef

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

attach
  :: forall m w s x
   . Monad m
  => Element              -- ^ Root DOM node
  -> (w ~> HtmlT s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)            -- ^ Evaluate application effects in @m@
  -> HtmlT s s m s      -- ^ Init action
  -> HtmlT s s m x      -- ^ Render action
  -> JSM x
attach rootEl runW runM init render = do
  let
    uninitializedDyn = error "Accessing dynamic variable before it was initialized"
    uninitializedEval = error "Accessing evaluation function before it was initialized"
  UnliftIO{..} <- askUnliftIO
  runRef <- liftIO (newIORef uninitializedEval)
  hteModel <- liftIO (newDynamicRef uninitializedDyn)
  hteSubscriber <- liftIO (newSubscriberRef (\w -> (readIORef runRef >>= ($ w))))
  let
    hteElement = ElementRef (pure rootEl) (\_ -> pure ())
    htmlEnv = HtmlEnv{..}
    runHtml :: forall a. HtmlT s s m a -> IO a
    runHtml = runM . flip runReaderT htmlEnv . runHtmlT
  liftIO $ writeIORef runRef (\(Exist h) -> void $ runHtml h)
  initial <- liftIO (runHtml init)
  liftIO $ drefModify hteModel \_ -> initial
  liftIO $ runHtml render

attachToBody
  :: Monad m
  => (w ~> HtmlT s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)            -- ^ Evaluate application effects in @m@
  -> HtmlT s s m s      -- ^ Init action
  -> HtmlT s s m x      -- ^ Render action
  -> JSM x
attachToBody runW runM init render = do
  rootEl <- jsg "document" ! "body"
  attach rootEl runW runM init render

attachSimple
  :: forall w s x
   . Element                -- ^ Root DOM node
  -> (w ~> HtmlT s s JSM) -- ^ Evaluate messages emitted by component
  -> HtmlT s s JSM s      -- ^ Init action
  -> HtmlT s s JSM x      -- ^ Render action
  -> JSM x
attachSimple rootEl runW init render = do
  UnliftIO{..} <- askUnliftIO
  attach rootEl runW unliftIO init render

attachToBodySimple
  :: (w ~> HtmlT s s JSM) -- ^ Evaluate messages emitted by component
  -> HtmlT s s JSM s      -- ^ Init action
  -> HtmlT s s JSM x      -- ^ Render action
  -> JSM x
attachToBodySimple runW init render = do
  UnliftIO{..} <- askUnliftIO
  attachToBody runW unliftIO init render

attachIO
  :: forall m w s x
   . Monad m
  => Element              -- ^ Root DOM node
  -> (w ~> HtmlT s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)            -- ^ Evaluate application effects in @m@
  -> HtmlT s s m s      -- ^ Init action
  -> HtmlT s s m x      -- ^ Render action
  -> IO ()
attachIO rootEl runW runM init render = withJSM $ attach rootEl runW runM init render

attachToBodyIO
  :: Monad m
  => (w ~> HtmlT s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)            -- ^ Evaluate application effects in @m@
  -> HtmlT s s m s      -- ^ Init action
  -> HtmlT s s m x      -- ^ Render action
  -> IO ()
attachToBodyIO runW runM init render = withJSM $ attachToBody runW runM init render

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
