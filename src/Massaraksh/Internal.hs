module Massaraksh.Internal where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.IORef
import Data.Foldable
import Data.Bool
import Data.List
import Language.Javascript.JSaddle
import Massaraksh.Event
import Massaraksh.Types
import Massaraksh.DOM

newElementRef :: Element -> Html ElementRef
newElementRef initial = do
  jsCtx <- asks he_js_context
  rootEl <- askElement
  elmRef <- liftIO (newIORef initial)
  liftJSM (appendChild rootEl initial)
  let
    read = readIORef elmRef
    write newEl = do
      oldEl <- readIORef elmRef
      runJSM (replaceChild rootEl newEl oldEl) jsCtx
      writeIORef elmRef newEl
  pure (ElementRef read write)

newElementRef' :: ElementRef -> JSM (ElementRef, IO ())
newElementRef' elRef = do
  flushedRef <- liftIO (newIORef False)
  frag <- liftJSM createDocumentFragment
  un <- askUnliftIO
  let
    read = readIORef flushedRef
      >>= bool (pure frag) (er_read elRef)
    write newEl = readIORef flushedRef
      >>= bool (pure ()) (er_write elRef newEl)
    flush = do
      writeIORef flushedRef True
      rootEl <- er_read elRef
      void $ unliftIO un $ liftJSM $ appendChild rootEl frag
  pure (ElementRef read write, flush)

askElement :: Html Element
askElement =
  liftIO =<< asks (er_read . he_element)
{-# INLINE askElement #-}

newtype RunJSM = RunJSM {unRunJSM :: forall x. JSM x -> IO x}

askRunJSM :: Html RunJSM
askRunJSM =  asks (\e -> RunJSM (\j -> runJSM j (he_js_context e)))
{-# INLINE askRunJSM #-}

writeElement :: Element -> Html ()
writeElement el = do
  elRef <- asks he_element
  liftIO $ er_write elRef el
{-# INLINE writeElement #-}

localElement :: Element -> Html a -> Html a
localElement elm child = do
  elRef <- newElementRef elm
  local (\env -> env { he_element = elRef }) child
{-# INLINE localElement #-}

htmlSubscribe :: Event a -> (a -> Reactive ()) -> Html (IO ())
htmlSubscribe e k = do
  subscriber <- asks (sub_unsubscriber . he_subscribe)
  liftIO $ sync (subscriber e k)

newSubscriber :: IO (Subscriber, Subscriptions)
newSubscriber = do
  subs <- newIORef []
  let
    subscriber = Subscriber \e f -> do
      unsub <- e `subscribe` f
      unRef <- liftIO (newIORef unsub)
      liftIO $ modifyIORef subs ((:) unRef)
      pure $ liftIO $ modifyIORef subs (delete unRef)
  pure (subscriber, subs)

subscribeUpdates :: Dynamic s -> Callback s -> Html (IO ())
subscribeUpdates d f = dyn_updates d `htmlSubscribe` f
{-# INLINE subscribeUpdates #-}

forDyn :: Dynamic a -> Callback a -> Html (IO ())
forDyn dyn k = do
  liftIO (dyn_read dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k
