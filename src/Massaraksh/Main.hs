-- FIXME: This module needs refactoring. Find simpler way to work with
-- recursive environments in ReaderT
{-# LANGUAGE CPP #-}
module Massaraksh.Main where

import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Language.Javascript.JSaddle
import Massaraksh.Base
import Massaraksh.Dynamic
import Massaraksh.Event
import Pipes as P
import qualified Data.Dynamic as D

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

type (~>) a b = forall x. a x -> b x

recPipe1 :: forall w m x. Monad m => (w ~> Producer1 w m) -> w x -> Effect m x
recPipe1 producer w = for (producer w) (\(Exists w) -> D.toDyn <$> recPipe1 producer w)

class HasRender s where
  renderL :: Prism' s ()

class HasInit a s | s -> a where
  initL :: Prism' s a

attach
  :: forall m w s x
   . JSVal                     -- ^ Root DOM node
  -> (w ~> ComponentT w s s m) -- ^ Evaluate messages emitted by component
  -> (m ~> IO)                 -- ^ Evaluate application specific effects in @m@
  -> ComponentT w s s m s      -- ^ Init action
  -> ComponentT w s s m x      -- ^ Render action
  -> JSM x
attach rootEl component extraEnv init render = do
  let uninitialized = error "Accessing dynamic state before it was initialized"
  hteDynamicRef@DynamicRef{..} <- liftIO (newDynamicRef uninitialized)
  UnliftIO{..} <- askUnliftIO
  let
    dobReplaceRoot = \_ -> pure ()
    dobRootNode    = pure rootEl
    hteDomBuilder  = DomBuilder{..}
    hteEventWriter = EventWriter \e -> void $ e `subscribe` runComponent
    hteExtraEnv    = extraEnv
    hteLiftJSM     = LiftJSM unliftIO
    hteSubscriber  = Subscriber \e f -> void $ e `subscribe` f
    env            = HtmlEnv{..}

    runComponent :: forall x. ComponentM e w s s x -> IO x
    runComponent =
      flip runReaderT env . runHtmlM . runEffect .
      flip for (\(Exists w) -> D.toDyn <$> recPipe1 component w)
  initial <- liftIO (runComponent init)
  liftIO (drefModify \_ -> initial)
  liftIO (runComponent render)

attachToBody
  :: (w ~> ComponentM e w s s)  -- ^ Evaluate messages emitted by component
  -> e                          -- ^ Extra application-specific environment
  -> ComponentM e w s s s       -- ^ Init action
  -> ComponentM e w s s x       -- ^ Render action
  -> JSM x
attachToBody component extraEnv init render = do
  rootEl <- jsg "document" ! "body"
  attach rootEl component extraEnv init render

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
