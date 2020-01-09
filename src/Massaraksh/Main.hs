-- FIXME: This module needs refactoring. Find simpler way to work with
-- recursive environments in ReaderT
{-# LANGUAGE CPP, RecursiveDo #-}
module Massaraksh.Main where

import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Natural
import Language.Javascript.JSaddle
import Massaraksh.Base
import Massaraksh.Dynamic
import Massaraksh.Event
import Massaraksh.Base
import Data.IORef
import Pipes as P
import qualified Data.Dynamic as D

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

recPipe1 :: forall w m x. Monad m => (w ~> Producer1 w m) -> w x -> Effect m x
recPipe1 producer w = for (producer w) (\(Exists w) -> D.toDyn <$> recPipe1 producer w)

attach
  :: forall m w s x
   . Monad m
  => JSVal                     -- ^ Root DOM node
  -> (w ~> ComponentT w s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)                 -- ^ Evaluate application specific effects in @m@
  -> ComponentT w s s m s      -- ^ Init action
  -> ComponentT w s s m x      -- ^ Render action
  -> JSM x
attach rootEl component runM init render = do
  let
    uninitializedDyn = error "Accessing dynamic variable before it was initialized"
    uninitializedEval = error "Accessing evaluation function before it was initialized"
  hteDynamicRef@DynamicRef{..} <- liftIO (newDynamicRef uninitializedDyn)
  runRef <- liftIO (newIORef uninitializedEval)
  UnliftIO{..} <- askUnliftIO
  SubscriberRef{..} <- liftIO (newSubscriberRef (\w -> readIORef runRef >>= ($ w)))
  let
    relmWrite     = \_ -> pure ()
    relmRead      = pure rootEl
    hteRootRef    = RootElmRef{..}
    hteSubscriber = sbrefValue
    env           = HtmlEnv{..}
    runComponent :: forall a. ComponentT w s s m a -> IO a
    runComponent =
      runM . flip runReaderT env . runHtmlT . runEffect .
      flip for (\(Exists w) -> D.toDyn <$> recPipe1 component w)
  liftIO (writeIORef runRef runComponent)
  initial <- liftIO (runComponent init)
  liftIO (drefModify \_ -> initial)
  liftIO (runComponent render)

attachToBody
  :: Monad m
  => (w ~> ComponentT w s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)                 -- ^ Evaluate application specific effects in @m@
  -> ComponentT w s s m s      -- ^ Init action
  -> ComponentT w s s m x      -- ^ Render action
  -> JSM x
attachToBody component runM init render = do
  rootEl <- jsg "document" ! "body"
  attach rootEl component runM init render

withJSM :: Maybe Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
withJSM _ = id
#else
withJSM explicitPort jsm = do
  envPort <- either (const Nothing) Just <$> try @SomeException (read <$> getEnv "PORT")
  progName <- getProgName
  let Just port = explicitPort <|> envPort <|> Just 8080
  let runWarp = Warp.run --if progName == "<interactive>" then Warp.debug else Warp.run
  putStrLn $ "Running jsaddle-warp application on http://localhost:" <> show port <> "/"
  runWarp port jsm
#endif
