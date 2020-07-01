module Massaraksh.Internal where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.IORef
import Data.List
import Data.Coerce
import Language.Javascript.JSaddle
import Massaraksh.Event
import Massaraksh.Types
import Massaraksh.DOM

newElementRef :: HtmlBase m => Element -> HtmlT m ElementRef
newElementRef initial = do
  rootEl <- askElement
  rootFrag <- askFragment
  frag <- liftJSM createDocumentFragment
  elementRef <- liftIO (newIORef initial)
  un <- lift askUnliftIO
  liftJSM (appendChild rootFrag initial)
  let
    read = readIORef elementRef
    readFrag = pure frag
    replace newEl = do
      oldEl <- readIORef elementRef
      void $ unliftIO un $ liftJSM (replaceChild rootEl newEl oldEl)
      writeIORef elementRef newEl
  pure (ElementRef read readFrag replace)

askElement :: HtmlBase m => HtmlT m Element
askElement =
  liftIO =<< asks (er_read . he_element)
{-# INLINE askElement #-}

askFragment :: HtmlBase m => HtmlT m Fragment
askFragment =
  liftIO =<< asks (er_fragment . he_element)
{-# INLINE askFragment #-}

writeElement :: HtmlBase m => Element -> HtmlT m ()
writeElement el = do
  elRef <- asks he_element
  liftIO $ er_write elRef el
{-# INLINE writeElement #-}

withNewChild :: HtmlBase m => Element -> HtmlT m ()
withNewChild elm = do
  void (newElementRef (coerce elm))
{-# INLINE withNewChild #-}

flushFragment :: HtmlBase m => HtmlT m ()
flushFragment = do
  rootEl <- askElement
  rootFrag <- askFragment
  liftJSM $ appendChild rootEl rootFrag
{-# INLINE flushFragment #-}

localElement
  :: HtmlBase m
  => Element
  -> HtmlT m a
  -> HtmlT m a
localElement elm child = do
  elRef <- newElementRef elm
  local (\env -> env { he_element = elRef }) child
{-# INLINE localElement #-}

htmlSubscribe
  :: HtmlBase m
  => Event a
  -> (a -> Reactive ())
  -> HtmlT m (IO ())
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

subscribeUpdates
  :: HtmlBase m
  => Dynamic s
  -> Callback s
  -> HtmlT m (IO ())
subscribeUpdates d f = do
  dyn_updates d `htmlSubscribe` f

forDyn :: HtmlBase m => Dynamic a -> Callback a -> HtmlT m (IO ())
forDyn dyn k = do
  liftIO (dyn_read dyn) >>= liftIO . sync . k
  subscribeUpdates dyn k
