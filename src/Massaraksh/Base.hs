{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List as L
import Data.String
import Data.Text as T hiding (index)
import Language.Javascript.JSaddle as JS
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: HtmlBase m => Text -> HtmlT s m x -> HtmlT s m x
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  localElement elm child

text :: HtmlBase m => Text -> HtmlT s m ()
text txt = do
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild textNode

dynText :: HtmlBase m => (s -> Text) -> HtmlT s m ()
dynText f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  dynUpdates `subscribePrivate` \Update{..} -> do
    void $ liftJSM $ textNode <# "nodeValue" $ f updNew
  appendChild textNode

prop :: (HtmlBase m, ToJSVal v) => Text -> v -> HtmlT s m ()
prop key val = do
  rootEl <- readElement
  liftJSM $ rootEl <# key $ val

(=:) :: HtmlBase m => Text -> Text -> HtmlT s m ()
(=:) = prop
infixr 7 =:

dynProp
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> (s -> v) -> HtmlT s m ()
dynProp key f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  rootEl <- readElement
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} -> liftJSM do
    oldProp <- fromJSValUnchecked =<< rootEl ! key
    when (f updNew /= oldProp) $
      liftJSM (rootEl <# key $ f updNew)

(~:)
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> (s -> v) -> HtmlT s m ()
(~:) = dynProp
infixr 7 ~:

attr :: HtmlBase m => Text -> Text -> HtmlT s m ()
attr key val = do
  rootEl <- readElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: HtmlBase m => Text -> (s -> Text) -> HtmlT s m ()
dynAttr key f = do
  Dynamic{..} <- asks (drefValue . hteModel)
  txt <- f <$> liftIO dynRead
  rootEl <- readElement
  liftJSM (rootEl <# key $ txt)
  void $ dynUpdates `subscribePrivate` \Update{..} ->
    void $ liftJSM $ rootEl # "setAttribute" $ (key, f updNew)

on
  :: HtmlBase m
  => Text
  -> Decoder (HtmlT s m x)
  -> HtmlT s m ()
on name decoder = do
  el <- readElement
  UnliftIO{..} <- askUnliftIO
  let
    event = Event \k -> do
      cb <- unliftIO $ liftJSM $ function \_ _ [event] -> do
        runDecoder decoder event >>= \case
          Left err  -> pure ()
          Right val -> liftIO (k val)
      unliftIO $ liftJSM (el # "addEventListener" $ (name, cb))
      pure $ unliftIO $ void $ liftJSM $ el # "removeEventListener" $ (name, cb)
  void $ subscribePublic event

on'
  :: HtmlBase m
  => Text
  -> HtmlT s m x
  -> HtmlT s m ()
on' name w = on name (pure w)

dynClassList :: HtmlBase m => [(Text, s -> Bool)] -> HtmlT s m ()
dynClassList xs =
  dynProp (T.pack "className") $
  \s -> T.unwords (L.foldl' (\acc (cs, f) -> if f s then cs:acc else acc) [] xs)

classList :: HtmlBase m => [(Text, Bool)] -> HtmlT s m ()
classList xs =
  prop (T.pack "className") $
  T.unwords (L.foldl' (\acc (cs, cond) -> if cond then cs:acc else acc) [] xs)

htmlFix :: (HtmlEval w s m -> HtmlEval w s m) -> HtmlEval w s m
htmlFix f = f (htmlFix f)

overHtml
  :: Functor m
  => Lens' s a
  -> HtmlT a m x
  -> HtmlT s m x
overHtml stab = localHtmlEnv \e -> let
  model = overDynamic stab (hteModel e)
  subscriber = flip contramap (hteSubscriber e) \(Exist html) ->
    Exist (overHtml stab html)
  in e { hteModel = model, hteSubscriber = subscriber }

composeHtml
  :: HtmlRec w a m
  -> HtmlRec w a m
  -> HtmlRec w a m
composeHtml next override yield = next (override yield)

interleaveHtml
  :: HtmlBase m
  => Lens' s a
  -> (HtmlLift s t a b m -> HtmlT a m x)
  -> HtmlT s m x
interleaveHtml stab interleave = do
  UnliftIO{..} <- askUnliftIO
  overHtml stab $ interleave (liftIO . unliftIO)

localHtmlEnv
  :: (HtmlEnv s m -> HtmlEnv a m)
  -> HtmlT a m x
  -> HtmlT s m x
localHtmlEnv f (HtmlT (ReaderT h)) = HtmlT $ ReaderT (h . f)

itraverseHtml
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> HtmlT (s, a) m ()
  -> HtmlT s m ()
itraverseHtml l child =
  itraverseInterleaveHtml l $ const (const child)

data ListItemRef s m = ListItemRef
  { lirHtmlEnv :: HtmlEnv s m
  , lirModify  :: (s -> s) -> IO ()
  }

itraverseInterleaveHtml
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> (Int -> HtmlInterleave s (s, a) m ())
  -> HtmlT s m ()
itraverseInterleaveHtml l interleave = do
  unliftH <- askUnliftIO
  unliftM <- lift askUnliftIO
  hte <- ask
  rootEl <- liftIO $ relmRead (hteElement hte)
  s <- liftIO $ dynRead $ drefValue (hteModel hte)
  itemRefs <- liftIO (newIORef [])
  let
    mkModifier :: Int -> Dynamic (s, a) -> ((s, a) -> (s, a)) -> IO ()
    mkModifier idx dyn f = do
      (_, oldA) <- dynRead dyn
      drefModify (hteModel hte) \oldS -> let
        (newS, newA) = f (oldS, oldA)
        in newS & iover l \i x -> if i == idx then newA else x

    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ListItemRef (s, a) m] -> [a] -> [a] -> m ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])   -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        subscriber <- liftIO $ newSubscriberRef \(Exist h) ->
          void $ unliftIO unliftM $ flip runReaderT newEnv $ runHtmlT h
        dynRef <- liftIO $ newDynamicRef (s, x)
        let
          model   = dynRef { drefModify = mkModifier idx (drefValue dynRef) }
          child   = interleave idx (liftIO . unliftIO unliftH)
          newEnv  = HtmlEnv (hteElement hte) model subscriber
          itemRef = ListItemRef newEnv (drefModify dynRef)
        flip runReaderT newEnv $ runHtmlT child
        liftIO (modifyIORef itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, []) -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        liftIO (writeIORef itemRefs newRefs)
        for_ tailRefs \ListItemRef{..} -> do
          subscriptions <- liftIO $ readIORef $ sbrefSubscriptions (hteSubscriber lirHtmlEnv)
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- liftJSM $ rootEl ! "childNodes" JS.!! idx
          liftJSM (rootEl # "removeChild" $ [childEl])
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ lirModify r \(oldS, oldA) -> (s, y)
        setup s (idx + 1) rs xs ys
      (_, _, _) -> do
        error "dynList: Incoherent internal state"

  lift $ setup s 0 [] [] (toListOf l s)
  let updates = dynUpdates . drefValue . hteModel $ hte
  updates `subscribePrivate` \Update{..} -> do
    refs <- liftIO (readIORef itemRefs)
    lift $ setup updNew 0 refs (toListOf l updOld) (toListOf l updNew)
  pure ()

instance (x ~ (), HtmlBase m) => IsString (HtmlT s m x) where
  fromString = text . T.pack
