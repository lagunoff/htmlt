{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}
module Massaraksh.Base where

import Control.Lens hiding ((#))
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.IORef
import Data.List as L
import Data.Text as T hiding (index)
import Language.Javascript.JSaddle as JS
import Massaraksh.Decode
import Massaraksh.Dynamic
import Massaraksh.Event
import Massaraksh.Internal
import Massaraksh.Types

el :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
el tag child = do
  elm <- liftJSM $ jsg "document" # "createElement" $ tag
  localElement elm child

text :: HtmlBase m => Text -> HtmlT m ()
text txt = do
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  appendChild textNode

dynText :: HtmlBase m => Dynamic Text -> HtmlT m ()
dynText d = do
  txt <- liftIO (dynamicRead d)
  textNode <- liftJSM $ jsg "document" # "createTextNode" $ txt
  dynamicUpdates d `subscribePrivate` \Update{..} -> do
    void $ liftJSM $ textNode <# "nodeValue" $ updNew
  appendChild textNode

prop :: (HtmlBase m, ToJSVal v) => Text -> v -> HtmlT m ()
prop key val = do
  rootEl <- askElement
  liftJSM $ rootEl <# key $ val

(=:) :: HtmlBase m => Text -> Text -> HtmlT m ()
(=:) = prop
infixr 3 =:

dynProp
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> Dynamic v -> HtmlT m ()
dynProp key dyn = do
  txt <- liftIO (dynamicRead dyn)
  rootEl <- askElement
  liftJSM (rootEl <# key $ txt)
  void $ dynamicUpdates dyn `subscribePrivate` \Update{..} -> liftJSM do
    oldProp <- fromJSValUnchecked =<< rootEl ! key
    when (updNew /= oldProp) $
      liftJSM (rootEl <# key $ updNew)

(~:)
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> Dynamic v -> HtmlT m ()
(~:) = dynProp
infixr 3 ~:

attr :: HtmlBase m => Text -> Text -> HtmlT m ()
attr key val = do
  rootEl <- askElement
  void $ liftJSM $ rootEl # "setAttribute" $ (key, val)

dynAttr :: HtmlBase m => Text -> Dynamic Text -> HtmlT m ()
dynAttr key dyn = do
  txt <- liftIO (dynamicRead dyn)
  rootEl <- askElement
  liftJSM (rootEl <# key $ txt)
  void $ dynamicUpdates dyn `subscribePrivate` \Update{..} ->
    void $ liftJSM $ rootEl # "setAttribute" $ (key, updNew)

on :: HtmlBase m => Text -> Decoder (HtmlT m x) -> HtmlT m ()
on name decoder = do
  el <- askElement
  un <- askUnliftIO
  let
    event = Event \k -> do
      cb <- unliftIO un $ liftJSM $ function \_ _ [event] -> do
        runDecoder decoder event >>= \case
          Left err  -> pure ()
          Right val -> liftIO (k val)
      unliftIO un $ liftJSM (el # "addEventListener" $ (name, cb))
      pure $ unliftIO un $ void $ liftJSM $ el # "removeEventListener" $ (name, cb)
  void $ subscribePublic event

on' :: HtmlBase m => Text -> HtmlT m x -> HtmlT m ()
on' name w = on name (pure w)

dynClassList :: HtmlBase m => [(Text, Dynamic Bool)] -> HtmlT m ()
dynClassList xs = do
  rootEl <- askElement
  let
    setup name = \case
      True  -> void $ liftJSM (rootEl ! "classList" # "add" $ name)
      False -> void $ liftJSM (rootEl ! "classList" # "remove" $ name)
  for_ xs \(name, dyn) -> do
    setup name =<< liftIO (dynamicRead dyn)
    subscribeUpdates dyn (setup name . updNew)

classList :: HtmlBase m => [(Text, Bool)] -> HtmlT m ()
classList xs =
  prop (T.pack "className") $
  T.unwords (L.foldl' (\acc (cs, cond) -> if cond then cs:acc else acc) [] xs)

localHtmlEnv
  :: (HtmlEnv m -> HtmlEnv m)
  -> HtmlT m x
  -> HtmlT m x
localHtmlEnv f (HtmlT (ReaderT h)) = HtmlT $ ReaderT (h . f)

withDynamicRef
  :: MonadIO m
  => s
  -> (DynamicRef s s -> HtmlT m x)
  -> HtmlT m x
withDynamicRef initial f =
  liftIO (newDynamicRef initial) >>= f

data ListItemRef s m = ListItemRef
  { lirHtmlEnv    :: HtmlEnv m
  , lirDynamicRef :: DynamicRef s s
  , lirModify     :: (s -> s) -> IO ()
  }

itraverseHtml
  :: forall s a m
   . HtmlBase m
  => IndexedTraversal' Int s a
  -> DynamicRef' s
  -> (Int -> DynamicRef' (s, a) -> HtmlT m ())
  -> HtmlT m ()
itraverseHtml l dynRef@(fromDynamicRef -> dyn) h = do
  unliftH <- askUnliftIO
  unliftM <- lift askUnliftIO
  hte <- ask
  rootEl <- askElement
  s <- liftIO (dynamicRefRead dynRef)
  itemRefs <- liftIO (newIORef [])
  let
    -- FIXME: 'setup' should return new contents for 'itemRefs'
    setup :: s -> Int -> [ListItemRef (s, a) m] -> [a] -> [a] -> m ()
    setup s idx refs old new = case (refs, old, new) of
      (_, [], [])    -> pure ()
      ([], [], x:xs) -> mdo
        -- New list is longer, append new elements
        subscriber <- liftIO $ newSubscriberRef \(Exist h) ->
          void $ unliftIO unliftM $ runHtmlT newEnv h
        dynRef' <- liftIO $ newDynamicRef (s, x)
        let
          model   = dynRef' {dynamicRefModify = mkModifier idx (fromDynamicRef dynRef')}
          newEnv  = HtmlEnv (htmlEnvElement hte) subscriber
          itemRef = ListItemRef newEnv model (dynamicRefModify dynRef')
        runHtmlT newEnv $ h idx model
        liftIO (modifyIORef itemRefs (<> [itemRef]))
        setup s (idx + 1) [] [] xs
      (_, x:xs, [])  -> do
        -- New list is shorter, delete the elements that no longer
        -- present in the new list
        itemRefsValue <- liftIO (readIORef itemRefs)
        let (newRefs, tailRefs) = L.splitAt idx itemRefsValue
        liftIO (writeIORef itemRefs newRefs)
        for_ tailRefs \ListItemRef{..} -> do
          subscriptions <- liftIO $ readIORef $
            subscriberRefSubscriptions (htmlEnvSubscriber lirHtmlEnv)
          liftIO $ for_ subscriptions (readIORef >=> id)
          childEl <- liftJSM $ rootEl ! "childNodes" JS.!! idx
          liftJSM (rootEl # "removeChild" $ [childEl])
      (r:rs, x:xs, y:ys) -> do
        -- Update child elemens along the way
        liftIO $ lirModify r \_ -> (s, y)
        setup s (idx + 1) rs xs ys
      (_, _, _)      -> do
        error "dynList: Incoherent internal state"

    mkModifier :: Int -> Dynamic (s, a) -> ((s, a) -> (s, a)) -> IO ()
    mkModifier idx dyn f = do
      (_, oldA) <- dynamicRead dyn
      dynamicRefModify dynRef \oldS -> let
        (newS, newA) = f (oldS, oldA)
        in newS & iover l \i x -> if i == idx then newA else x

  lift $ setup s 0 [] [] (toListOf l s)
  subscribeUpdates dyn \Update{..} -> do
    refs <- liftIO (readIORef itemRefs)
    lift $ setup updNew 0 refs (toListOf l updOld) (toListOf l updNew)
  pure ()

eitherHtml
  :: forall s l r m
   . HtmlBase m
  => Lens' s (Either l r)
  -> DynamicRef' s
  -> (DynamicRef' (s, l) -> HtmlT m ())
  -> (DynamicRef' (s, r) -> HtmlT m ())
  -> HtmlT m ()
eitherHtml lens dynRef@(fromDynamicRef -> dyn) left right = do
  unliftM <- lift askUnliftIO
  env <- ask
  initial <- liftIO (dynamicRefRead dynRef)
  elRef <- asks htmlEnvElement
  leftEnvRef <- liftIO $ newIORef Nothing
  rightEnvRef <- liftIO $ newIORef Nothing
  leftSubscriber <- liftIO $ newSubscriberRef \(Exist h) -> do
    mayLir <- readIORef leftEnvRef
    for_ (lirHtmlEnv <$> mayLir) \env ->
      unliftIO unliftM $ runHtmlT env h
  rightSubscriber <- liftIO $ newSubscriberRef \(Exist h) -> do
    mayLir <- readIORef rightEnvRef
    for_ (lirHtmlEnv <$> mayLir) \env ->
      unliftIO unliftM $ runHtmlT env h
  let
    setup s = \case
      Left l -> liftIO (readIORef leftEnvRef) >>= \case
        Nothing -> do
          dynRef' <- liftIO $ newDynamicRef (s, l)
          let
            hteModel = dynRef' {dynamicRefModify = leftModifier (fromDynamicRef dynRef')}
            env      = HtmlEnv elRef leftSubscriber
          liftIO $ writeIORef leftEnvRef $ Just $
            ListItemRef env hteModel (dynamicRefModify dynRef')
          void $ runHtmlT env $ left hteModel
        Just env -> liftIO do
          lirModify env \_ -> (s, l)
      Right r -> liftIO (readIORef rightEnvRef) >>= \case
        Nothing -> do
          dynRef' <- liftIO $ newDynamicRef (s, r)
          let
            hteModel = dynRef' {dynamicRefModify = rightModifier (fromDynamicRef dynRef')}
            env      = HtmlEnv elRef rightSubscriber
          liftIO $ writeIORef rightEnvRef $ Just $
            ListItemRef env hteModel (dynamicRefModify dynRef')
          void $ runHtmlT env $ right hteModel
        Just env -> liftIO do
          lirModify env \_ -> (s, r)

    leftModifier :: Dynamic (s, l) -> ((s, l) -> (s, l)) -> IO ()
    leftModifier dyn f = do
      (_, oldL) <- dynamicRead dyn
      dynamicRefModify dynRef \oldS -> let
        (newS, newL) = f (oldS, oldL)
        in newS & lens .~ Left newL

    rightModifier :: Dynamic (s, r) -> ((s, r) -> (s, r)) -> IO ()
    rightModifier dyn f = do
      (_, oldR) <- dynamicRead dyn
      dynamicRefModify dynRef \oldS -> let
        (newS, newR) = f (oldS, oldR)
        in newS & lens .~ Right newR

    -- FIXME: that constrains usage of 'eitherHtml' to the cases when
    -- it is the only children of some HTML element because all
    -- sibling will be removed when tag is switched. Alternative way
    -- could be: keep track of what elements were appended during
    -- initialization and remove only them
    removeAllChilds = askElement >>= \rootEl -> liftJSM do
      length <- fromJSValUnchecked =<< rootEl ! "childNodes" ! "length"
      for_ [0..length - 1] \idx -> do
        childEl <- rootEl ! "childNodes" JS.!! idx
        rootEl # "removeChild" $ [childEl]

  lift $ setup initial (initial ^. lens)
  void $ subscribeUpdates dyn \Update{..} -> do
    let (old, new) = (updOld ^. lens, updNew ^. lens)
    when (isLeft old && isRight new) $ removeAllChilds *> liftIO do
      readIORef leftEnvRef >>= flip for_ (htmlFinalize . lirHtmlEnv)
      writeIORef leftEnvRef Nothing
    when (isRight old && isLeft new) $ removeAllChilds *> liftIO do
      readIORef rightEnvRef >>= flip for_ (htmlFinalize . lirHtmlEnv)
      writeIORef rightEnvRef Nothing
    lift $ setup updNew new
