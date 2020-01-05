-- FIXME: This module needs refactoring. Find simpler way to work with
-- recursive environments in ReaderT
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE CPP, RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Massaraksh.Main where

import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Typeable
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

withJSM :: Maybe Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
withJSM _ = id
#else
withJSM explicitPort jsm = do
  envPort <- either (const Nothing) Just <$> try @SomeException (read <$> getEnv "PORT")
  progName <- getProgName
  let Just port = explicitPort <|> envPort <|> Just 8080
  let runWarp = if progName == "<interactive>" then Warp.debug else Warp.run
  putStrLn $ "Running jsaddle-warp application on http://localhost:" <> show port <> "/"
  runWarp port jsm
#endif

newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

recPipe :: Monad m => (w -> Producer w m ()) -> w -> Effect m ()
recPipe producer w = for (producer w) (recPipe producer)

recPipe1 :: forall w m x. Monad m => (forall x. w x -> Producer1 w m x) -> w x -> Effect m x
recPipe1 producer w = for (producer w) (\(Exists w) -> D.toDyn <$> recPipe1 producer w)

class HasRender a s where
  renderL :: Prism' s a

class HasInit a s | s -> a where
  initL :: Prism' s a

attach
  :: forall e d x w
   . (HasHtmlEnv d (w ()) (Fix e))
  => JSVal
  -> (HtmlEnv d (w ()) -> Fix e)
  -> (forall x. w x -> HtmlM (Fix e) x)
  -> d
  -> HtmlM (Fix e) x
  -> JSM x
attach rootEl mkEnv handle initial html = do
  dh@DynamicHandle{..} <- liftIO (newDynamic initial)
  UnliftIO{..} <- askUnliftIO
  let
    hteBuilder   = newRootBuilder rootEl
    hteDynamic   = dh
    hteLiftJSM   = LiftJSM unliftIO
    hteSubscribe = Subscribe \e f -> void $ e `_evSubscribe` f
    hteTellEvent = TellEvent \e -> void $ e `_evSubscribe` (flip runReaderT env . runHtmlM . handle)
    env          = mkEnv HtmlEnv{..}
  liftIO . flip runReaderT env . runHtmlM $ html
  where
    newRootBuilder rootEl =
      let
        dobAskRoot       = pure rootEl
        dobReplaceRoot _ = pure ()
      in DomBuilder{..}

attachToBody
  :: forall e d x w
   . (HasHtmlEnv d (w ()) (Fix e))
  => (HtmlEnv d (w ()) -> Fix e)
  -> (forall x. w x -> HtmlM (Fix e) x)
  -> d
  -> HtmlM (Fix e) x
  -> JSM x
attachToBody mkEnv handle initial html = do
  rootEl <- jsg "document" ! "body"
  attach rootEl mkEnv handle initial html

attachComponent
  :: forall e d w f
   . (HasHtmlEnv d (Producer1 w (HtmlM (Fix e)) ()) (Fix e), HasRender () (w ()), HasInit f (w d), Typeable d)
  => JSVal
  -> (HtmlEnv d (Producer1 w (HtmlM (Fix e)) ()) -> Fix e)
  -> (forall x. w x -> Producer1 w (HtmlM (Fix e)) x)
  -> f
  -> JSM ()
attachComponent rootEl mkEnv handle flags = mdo
  let
    runEvent :: forall x. w x -> IO x
    runEvent = flip runReaderT hte . runHtmlM . runEffect . recPipe1 handle
    runEvent2 :: forall x. Producer1 w (HtmlM (Fix e)) x -> IO x
    runEvent2 = flip runReaderT hte . runHtmlM . runEffect . flip for (\(Exists w) -> D.toDyn <$> recPipe1 handle w)
  initial <- liftIO $ runEvent (flags ^. re initL)
  dh@DynamicHandle{..} <- liftIO (newDynamic initial)
  UnliftIO{..} <- askUnliftIO
  let
    hteBuilder   = newRootBuilder rootEl
    hteDynamic   = dh
    hteLiftJSM   = LiftJSM unliftIO
    hteSubscribe = Subscribe \e f -> void $ e `_evSubscribe` f
    hteTellEvent = TellEvent \e -> void $ e `_evSubscribe` runEvent2
    hte          = mkEnv $ HtmlEnv{..}
  liftIO $ runEvent (() ^. re renderL)
  where
    newRootBuilder rootEl =
      let
        dobAskRoot       = pure rootEl
        dobReplaceRoot _ = pure ()
      in DomBuilder{..}

attachComponentToBody
  :: forall e d w f
   . (HasHtmlEnv d (Producer1 w (HtmlM (Fix e)) ()) (Fix e), HasRender () (w ()), HasInit f (w d), Typeable d)
  => (HtmlEnv d (Producer1 w (HtmlM (Fix e)) ()) -> Fix e)
  -> (forall x. w x -> Producer1 w (HtmlM (Fix e)) x)
  -> f
  -> JSM ()
attachComponentToBody mkEkv handle initial = do
  rootEl <- jsg "document" ! "body"
  attachComponent rootEl mkEkv handle initial

newtype ComponentEnvF d w f = ComponentEnvF
  { unComponentEnv :: HtmlEnv d (Producer1 w (HtmlM f) ()) }

newtype HtmlEnvF d f = HtmlEnvF
  { unHtmlEnvF :: HtmlEnv d (HtmlM f ()) }

instance HasHtmlEnv d (HtmlM (Fix (HtmlEnvF d)) ()) (Fix (HtmlEnvF d)) where
  htmlEnvL = iso (unHtmlEnvF . unfix) (Fix . HtmlEnvF)

instance HasHtmlEnv d (Producer1 w (HtmlM (Fix (ComponentEnvF d w))) ()) (Fix (ComponentEnvF d w)) where
  htmlEnvL = iso (unComponentEnv . unfix) (Fix . ComponentEnvF)

instance HasHtmlEnv d (Producer1 w (HtmlM (Fix (ComponentEnvF d w))) ()) (ComponentEnvF d w (Fix (ComponentEnvF d w))) where
  htmlEnvL = iso (unComponentEnv) (ComponentEnvF)

type ComponentM o e a = Producer1 o (HtmlM e) a
