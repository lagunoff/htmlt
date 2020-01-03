{-# LANGUAGE CPP, RecursiveDo #-}
module Massaraksh.Main where

import Control.Monad.Reader
import Language.Javascript.JSaddle
import Pipes as P
import Control.Monad.IO.Unlift
import Massaraksh.Event
import Massaraksh.Dynamic
import Massaraksh.Base

#ifndef ghcjs_HOST_OS
import Control.Applicative ((<|>))
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

attachTo
  :: (e ~ HtmlEnv model msg)
  => JSVal
  -> (msg -> Producer msg (HtmlM e) ())
  -> HtmlM e a
  -> model
  -> JSM a
attachTo rootEl handle mkHtml initial = do
  dh@DynamicHandle{..} <- liftIO (newDynamic initial)
  UnliftIO{..} <- askUnliftIO
  let
    runEvent     = flip runReaderT hte . runHtmlM . runEffect . recPipe handle
    hteBuilder   = newRootBuilder rootEl
    hteDynamic   = dh
    hteLiftJSM   = LiftJSM unliftIO
    hteSubscribe = Subscribe \e f -> void $ e `_evSubscribe` f
    hteTellEvent = TellEvent \e -> void $ e `_evSubscribe` runEvent
    hte          = HtmlEnv{..}
  liftIO $ flip runReaderT hte $ runHtmlM mkHtml
  where
    newRootBuilder rootEl =
      let
        dobAskRoot       = pure rootEl
        dobReplaceRoot _ = pure ()
      in DomBuilder{..}

attachToBody
  :: (e ~ HtmlEnv model w)
  => (w -> Producer w (HtmlM e) ())
  -> HtmlM e a
  -> model
  -> JSM a
attachToBody handle html initial = do
  rootEl <- jsg "document" ! "body"
  attachTo rootEl handle html initial

attachToBodySimple
  :: (e ~ HtmlEnv () w)
  => (w -> Producer w (HtmlM e) ())
  -> HtmlM e a
  -> JSM a
attachToBodySimple handle html =
  attachToBody handle html ()

withJSM :: Maybe Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
withJSM _ = id
#else
withJSM explicitPort jsm = do
  envPort <- either (const Nothing) Just <$> try @SomeException (read <$> getEnv "PORT")
  progName <- getProgName
  let Just port = explicitPort <|> envPort <|> Just 8080
  let runWarp = Warp.run -- if progName == "<interactive>" then Warp.debug else Warp.run
  putStrLn $ "Running jsaddle-warp application on http://localhost:" <> show port <> "/"
  runWarp port jsm
#endif

recPipe :: Monad m => (w -> Producer w m ()) -> w -> Effect m ()
recPipe producer w = for (producer w) (recPipe producer)

-- type Handler1 w m = forall x y. w x -> Proxy X () y (w y) m x
type Handler1 w m = forall x. w x -> Producer1 w m x

recPipe1 :: forall w m x. Monad m => Handler1 w m -> w x -> Effect m x
recPipe1 producer w = for (producer w) (\(Exists w) -> Existed w <$> recPipe1 producer w)

class HasRender a where
  isRender :: a x -> Bool
  buildRender :: a ()

class HasInit f d a | a -> d, d -> f where
  buildInit :: f -> a d

attachComponent
  :: forall e d w f. (e ~ HtmlEnv d (w ()), HasRender w, HasInit f d w)
  => JSVal
  -> (Handler1 w (HtmlM e))
  -> f
  -> JSM ()
attachComponent rootEl handle flags = mdo
  let
    runEvent :: forall x. w x -> IO x
    runEvent = flip runReaderT hte . runHtmlM . runEffect . recPipe1 handle
  initial <- liftIO $ runEvent (buildInit flags)
  dh@DynamicHandle{..} <- liftIO (newDynamic initial)
  UnliftIO{..} <- askUnliftIO
  let
    hteBuilder   = newRootBuilder rootEl
    hteDynamic   = dh
    hteLiftJSM   = LiftJSM unliftIO
    hteSubscribe = Subscribe \e f -> void $ e `_evSubscribe` f
    hteTellEvent = TellEvent \e -> void $ e `_evSubscribe` runEvent
    hte          = HtmlEnv{..}
  void $ liftIO $ runEvent buildRender
  where
    newRootBuilder rootEl =
      let
        dobAskRoot       = pure rootEl
        dobReplaceRoot _ = pure ()
      in DomBuilder{..}

attachComponentToBody
  :: forall e d w f. (e ~ HtmlEnv d (w ()), HasRender w, HasInit f d w)
  => (Handler1 w (HtmlM e))
  -> f
  -> JSM ()
attachComponentToBody handle initial = do
  rootEl <- jsg "document" ! "body"
  attachComponent rootEl handle initial
