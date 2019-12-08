{-# LANGUAGE CPP                    #-}
{-# LANGUAGE MultiWayIf             #-}
module Massaraksh.Html.Base where

import Control.Applicative ((<|>))
import Control.Lens ((&), Const(..), Lens, (%~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first, second)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Text (Text)
import Data.Traversable (for)
import Language.Javascript.JSaddle
import Massaraksh
import Massaraksh.Html.Decoder
import Prelude hiding ((!!))
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Massaraksh.Dynamic as Dynamic
import qualified Prelude as Prelude
import Pipes hiding (for)

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

type Html msg input output = UI JSM JSVal msg input output

type Html2 input output m = UI' JSVal input output m
newtype Html3 input output m = Html3 { runHTML3 :: MonadJSM m => UI' JSVal input output m }

el2 :: MonadJSM m => Text -> [Html3 input output m] -> Html3 input output m
el2 name ch = Html3 $ liftJSM do
  pure (UIResult undefined undefined undefined)

type Attr2 input output m = JSVal -> m (Pipe input  output m ())

newtype Attr msg i o = Attr
  { _runAttr
    :: Dynamic JSM i
    -> Sink JSM (AttrMsg msg i o)
    -> JSVal
    -> JSM (JSM ()) }

data AttrMsg msg input output
  = AStep { _asModify :: input -> output }
  | AYield { _ayMessage :: msg }

toUIMesage :: AttrMsg msg i o -> UIMsg widget msg i o
toUIMesage = \case
  AStep{..}  -> Step _asModify
  AYield{..} -> Yield _ayMessage

noopAttr :: Attr msg i o
noopAttr = Attr \_ _ _ -> pure $ pure ()

text :: Text -> Html msg i o
text content = UI \_ _ -> do
  el <- jsg "document" # "createTextNode" $ content
  pure $ UIHandle el $ pure ()

lazyText :: LT.Text -> Html msg i o
lazyText content = UI \_ _ -> do
  -- FIXME: this throws exception in runtime with GHCJS + reflex-platform
  -- ui <- DOM.toNode <$> DOM.createTextNode doc (JSS.lazyTextToJSString content)
  ui <- jsg "document" # "createTextNode" $ LT.toStrict content
  pure $ UIHandle ui $ pure ()

el
  :: Text
  -> [Attr msg i o]
  -> [Html msg i o]
  -> Html msg i o
el tag attrs childs = UI \model sink -> do
  el <- jsg "document" # "createElement" $ tag
  let
    applyAttr (Attr setup) = setup model (sink . toUIMesage) el
    childSink idx = \case
      (Step io)     -> sink (Step io)
      (Yield msg)   -> sink (Yield msg)
      (Ref newNode) -> do
        oldNode <- el ! "children" !! idx
        void $ el # "replaceChild" $ (newNode, oldNode)

  attrFinalizers <- for attrs applyAttr
  childFinalizers <- for (zip [0..] childs) \(idx, ch) -> do
    UIHandle{..} <- runUI ch model (childSink idx)
    el # "appendChild" $ _uihWidget
    pure _uihFinalize
  pure $ UIHandle el (sequence_ attrFinalizers *> sequence_ childFinalizers)

unsafeHtml :: Text -> [Attr msg i o] -> Text -> Html msg i o
unsafeHtml name other html =
  el name (prop (T.pack "innerHTML") html:other) []

list
  :: forall msg s t a b
   . Lens s t [b] [b]
  -> Text
  -> [Attr msg s t]
  -> Html (Int -> msg) a b
  -> (s -> b -> a)
  -> Html msg s t
list stbb tag attrs child props = UI \store sink -> do
  el <- jsg "document" ^. js1 "createElement" tag
  s <- _dynRead store
  let alist = getConst (stbb Const s)
  itemFinalizers <- liftIO $ newIORef []
  let
    getLastChild :: JSVal -> JSM (Maybe JSVal)
    getLastChild el = el ! "lastChild" >>= fromJSVal

    removeChild_ :: JSVal -> JSVal -> JSM ()
    removeChild_ el ch = void $ el # "removeChild" $ ch

    removeLastNChildren :: Int -> JSVal -> JSM ()
    removeLastNChildren n node = go n
      where
        go 0 = pure ()
        go c = getLastChild node >>= \case
          Just ch -> removeChild_ node ch *> go (c - 1)
          Nothing -> pure ()

    applyAttr :: Attr msg s t -> JSM (JSM ())
    applyAttr (Attr setup) = setup store (sink . toUIMesage) el

    modifyAt :: forall a. Int -> (a -> a) -> [a] -> [a]
    modifyAt _ _ [] = []
    modifyAt idx f (x:xs)
      | idx == 0 = f x : xs
      | otherwise = x : modifyAt (idx - 1) f xs

    itemSink idx (Step f) = sink $ Step \s -> s & stbb %~ modifyAt idx (f . props s)
    itemSink idx (Yield f) = sink $ Yield (f idx)
    itemSink idx (Ref newNode) = do
      oldNode <- el ! "children" !! idx
      void $ el # "replaceChild" $ [newNode, oldNode]

    childFinalizer = do
      handles <- liftIO $ readIORef itemFinalizers
      sequence_ handles
      liftIO $ writeIORef itemFinalizers []

    nth :: forall a. Int -> [a] -> Maybe a
    nth _ []       = Nothing
    nth 0 (x : _)  = Just x
    nth n (_ : xs) = nth (n - 1) xs

    setup :: [b] -> [b] -> JSM ()
    setup old new =
      if | length new > length old -> do
             for_ [length old .. (length new - 1)] \idx -> do
               s <- _dynRead store
               let defaultA = props s $ new Prelude.!! (length old + idx)
               let projMaybeA s = props s <$> nth idx (getConst (stbb Const s))
               let itemDynamic = Dynamic.mapMaybe defaultA projMaybeA store
               UIHandle node finalizer <- runUI child itemDynamic $ itemSink idx
               el # "appendChild" $ [node]
               liftIO $ modifyIORef itemFinalizers (finalizer :)
         | length new < length old -> do
             let d = length old - length new
             dropped <- liftIO $ atomicModifyIORef' itemFinalizers (\xs -> (drop d xs, take d xs))
             sequence_ dropped
             removeLastNChildren d el
         | otherwise -> pure ()

  unsubscribe <- _dynUpdates store `_evSubscribe` \Update{..} ->
    setup (getConst (stbb Const _updOld)) (getConst (stbb Const _updNew))
  attrFinalizers <- for attrs applyAttr
  setup [] alist
  pure $ UIHandle el (childFinalizer *> sequence_ attrFinalizers *> unsubscribe)

eitherHtml
  :: Html msg i1 i1
  -> Html msg i2 i2
  -> Html msg (Either i1 i2) (Either i1 i2)
eitherHtml left right = UI \store sink -> do
  model <- _dynRead store
  leftHandle <- liftIO $ newIORef Nothing
  rightHandle <- liftIO $ newIORef Nothing

  unsubscribe <- _dynUpdates store `_evSubscribe` \case
    Update (Left old) (Right new) -> (liftIO (readIORef leftHandle)) >>= \case
      Just (UIHandle _ leftFinalizer) -> do
        leftFinalizer
        liftIO $ writeIORef leftHandle Nothing
        handle <- setupRight store sink new
        sink $ Ref (_uihWidget handle)
        liftIO $ writeIORef rightHandle (Just handle)
      Nothing -> pure () -- impossible
    Update (Right old) (Left new) -> (liftIO (readIORef rightHandle)) >>= \case
      Just (UIHandle _ rightFinalizer) -> do
        rightFinalizer
        liftIO $ writeIORef rightHandle Nothing
        handle <- setupLeft store sink new
        sink $ Ref (_uihWidget handle)
        liftIO $ writeIORef leftHandle (Just handle)
      Nothing -> pure () -- impossible
    Update _ _ -> pure ()
  widget <- case model of
    Right r -> do
      handle <- setupRight store sink r
      liftIO $ writeIORef rightHandle (Just handle)
      pure (_uihWidget handle)
    Left l -> do
      handle <- setupLeft store sink l
      liftIO $ writeIORef leftHandle (Just handle)
      pure (_uihWidget handle)

  let
    finalizer = unsubscribe
      *> (liftIO (readIORef rightHandle) >>= unsubscribeMaybe)
      *> (liftIO (readIORef leftHandle) >>= unsubscribeMaybe)

  pure (UIHandle widget finalizer)
  where
    unsubscribeMaybe = maybe (pure ()) _uihFinalize
    setupLeft store sink model =
      runUI left (Dynamic.mapMaybe model (either Just (const Nothing)) store) sinkLeft
        where
          sinkLeft (Yield msg)  = sink (Yield msg)
          sinkLeft (Step io)    = sink (Step (first io))
          sinkLeft (Ref widget) = sink (Ref widget)
    setupRight store sink model =
      runUI right (Dynamic.mapMaybe model (either (const Nothing) Just) store) sinkRight
        where
          sinkRight (Yield msg)  = sink (Yield msg)
          sinkRight (Step io)    = sink (Step (second io))
          sinkRight (Ref widget) = sink (Ref widget)

-- ** Element properties

prop :: ToJSVal val => Text -> val -> Attr msg i o
prop name val = Attr \_ _ el -> do
  jsval <- toJSVal val
  setProp (toJSString name) jsval (unsafeCoerce el)
  pure $ pure ()

attr :: Text -> Text -> Attr msg i o
attr name val = Attr \_ _ el -> do
  el <# name $ val
  pure $ pure ()

-- ** Attaching events to elements

on
  :: Text
  -> (i -> AttrMsg msg i o)
  -> Attr msg i o
on eventName mkMessage =
  onWithDecoder eventName emptyDecoder (\i _ -> mkMessage i)

on_
  :: Text
  -> msg
  -> Attr msg i o
on_ eventName msg = on eventName (const . AYield $ msg)

onWithDecoder
  :: Text
  -> Decoder a
  -> (i -> a -> AttrMsg msg i o)
  -> Attr msg i o
onWithDecoder eventName decoder mkMessage = Attr setup
  where
    setup model sink el = do
      cb <- function \_ _ [event] -> do
        latestModel <- _dynRead model
        result <- runDecoder decoder event
        case result of
          Right val -> sink (mkMessage latestModel val)
          Left err  -> pure ()
      el # "addEventListener" $ (eventName, cb)
      pure $ void $ el # "removeEventListener" $ (eventName, cb)

onWithDecoder_
  :: Text
  -> Decoder a
  -> (a -> msg)
  -> Attr msg i o
onWithDecoder_ eventName decoder mkMessage
  = onWithDecoder eventName decoder (\_ -> AYield . mkMessage)

onInput :: (i -> Text -> AttrMsg msg i o) -> Attr msg i o
onInput = onWithDecoder (T.pack "input") valueDecoder

onInput_ :: (Text -> msg) -> Attr msg i o
onInput_ mkMessage = onWithDecoder (T.pack "input") valueDecoder \_ -> AYield . mkMessage

-- ** Some synomyms for 'prop'

boolProp :: Text -> Bool -> Attr msg i o
boolProp = prop

stringProp :: Text -> Text -> Attr msg i o
stringProp = prop

textProp :: Text -> Text -> Attr msg i o
textProp = prop

intProp :: Text -> Int -> Attr msg i o
intProp = prop

doubleProp ::  Text -> Double -> Attr msg i o
doubleProp = prop

-- ** Bootstrapping utilities

data AppHandle msg model = AppHandle
  { appHandleReadModel :: JSM model
  , appHandleStep      :: (model -> model) -> JSM ()
  , appHandleSend      :: msg -> JSM ()
  , appHandleDetach    :: JSM ()
  }

data Config msg model = Config
  { configInit  :: JSM model
  , configView  :: Html msg model model
  , configEval  :: AppHandle msg model -> msg -> JSM ()
  , configSetup :: AppHandle msg model -> JSM ()
  , configPort  :: Maybe Int
  }

defaultConfig
  :: Html msg model model
  -> model
  -> Config msg model
defaultConfig configView model = Config {..}
  where
    configInit = pure model
    configEval = ignoreMessages
    configSetup = ignoreSetup
    configPort = Nothing

attachToNode
  :: JSVal
  -> Config msg model
  -> JSM ()
attachToNode parent (Config init view eval setup _) = do
  model <- init
  storeHandle <- createDynamic model
  appHandleRef <- liftIO $ newIORef Nothing
  nodeRef <- liftIO $ newIORef Nothing
  let
    sink (Yield msg) = liftIO (readIORef appHandleRef) >>= \case
      Just appHandle -> eval appHandle msg
      Nothing        -> pure ()
    sink (Step io) = _dhModify storeHandle io
    sink (Ref newNode) = liftIO (readIORef nodeRef) >>= \case
      Just oldNode -> do
        liftIO $ writeIORef nodeRef (Just newNode)
        void $ parent # "replaceChild" $ [newNode, oldNode]
        pure ()
      Nothing      -> pure ()
  UIHandle node finalizer <- runUI view (_dhDynamic storeHandle) sink
  let appHandle = AppHandle (_dynRead $ _dhDynamic storeHandle) (sink . Step) (sink . Yield) finalizer

  liftIO $ writeIORef appHandleRef (Just appHandle)
  liftIO $ writeIORef nodeRef (Just node)
  void $ parent # "appendChild" $ [ node ]
  setup appHandle

attachToBody :: Config msg model -> JSM ()
attachToBody config = do
  body <- jsg "document" ! "body"
  attachToNode body config

ignoreMessages = const . const . pure $ ()
ignoreSetup = const . pure $ ()

defaultMain
  :: Html msg model model
  -> model
  -> IO ()
defaultMain view model = defaultMainWith (defaultConfig view model)

defaultMainWith :: Config msg model -> IO ()
defaultMainWith config = withJSM (configPort config) (attachToBody config)

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
