{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
module Massaraksh.Html.Core where

import Control.Applicative ((<|>))
import Control.Lens ((&), Const(..), Lens, (%~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Text (Text)
import Data.Traversable (for)
import GHCJS.DOM.Element (setAttribute)
import GHCJS.DOM.EventM (EventName)
import GHCJS.DOM.Node (appendChild_, setTextContent, toNode, removeChild_, getLastChild)
import GHCJS.DOM.Types (Node, JSM, Element(..), HTMLElement(..), ToJSVal(..), IsEvent, uncheckedCastTo, ToJSString(..), liftJSM)
import Language.Javascript.JSaddle (setProp)
import Massaraksh
import Massaraksh.Html.Decoder
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.JSString.Text as JSS
import qualified Data.Text.Lazy as LT
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.GlobalEventHandlers as E
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified Massaraksh.Store as Store

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Warp as Warp
import System.Environment
import Control.Exception
#endif

type Html msg input output = UI JSM Node msg input output

newtype Attribute msg i o = Attribute
  { runAttr
      :: Store JSM i
      -> Sink JSM (Either msg (i -> o))
      -> HTMLElement
      -> JSM (JSM ())
  }

noopAttr :: Attribute msg i o
noopAttr = Attribute \_ _ _ -> pure $ pure ()

textDyn :: (i -> Text) -> Html msg i o
textDyn f = UI \model _ -> do
  doc <- DOM.currentDocumentUnchecked
  content <- f <$> readStore model
  ui <- DOM.toNode <$> DOM.createTextNode doc (JSS.textToJSString content)
  unsubscribe <- updates model `subscribe` \Updates {new} ->
    setTextContent ui (Just (f new))
  pure (UIHandle ui unsubscribe)

text :: Text -> Html msg i o
text content = UI \_ _ -> do
  doc <- DOM.currentDocumentUnchecked
  ui <- DOM.toNode <$> DOM.createTextNode doc content
  pure $ UIHandle ui $ pure ()

lazyText :: LT.Text -> Html msg i o
lazyText content = UI \_ _ -> do
  doc <- DOM.currentDocumentUnchecked
  -- FIXME: this throws exception in runtime with GHCJS + reflex-platform
  -- ui <- DOM.toNode <$> DOM.createTextNode doc (JSS.lazyTextToJSString content)
  ui <- DOM.toNode <$> DOM.createTextNode doc (LT.toStrict content)
  pure $ UIHandle ui $ pure ()

el
  :: Text
  -> [Attribute msg i o]
  -> [Html msg i o]
  -> Html msg i o
el tag attrs childs = UI \model sink -> do
  doc <- DOM.currentDocumentUnchecked
  el <- DOM.uncheckedCastTo HTMLElement <$> DOM.createElement doc tag
  let sinkAttr = either (sink . Yield) (sink . Step)
      applyAttr (Attribute setup) = setup model sinkAttr el
  attrFinalizers <- for attrs applyAttr
  childFinalizers <- for childs \ch -> do
    UIHandle ui finalizer <- unUI ch model sink
    appendChild_ el ui
    pure finalizer
  pure $ UIHandle (toNode el) (sequence_ attrFinalizers *> sequence_ childFinalizers)

list
  :: forall msg s t a b
   . Lens s t [b] [b]
  -> Text
  -> [Attribute msg s t]
  -> Html (Int -> msg) a b
  -> (s -> b -> a)
  -> Html msg s t
list stbb tag attrs child props = UI \store sink -> do
  doc <- DOM.currentDocumentUnchecked
  el <- DOM.uncheckedCastTo HTMLElement <$> DOM.createElement doc tag
  s <- readStore store
  let alist = getConst (stbb Const s)
  itemFinalizers <- liftIO $ newIORef []
  let sinkAttr :: Sink JSM (Either msg (s -> t))
      sinkAttr = either (sink . Yield) (sink . Step)

      removeLastNChildren :: Int -> Node -> JSM ()
      removeLastNChildren n node = go n
        where
          go 0 = pure ()
          go c = getLastChild node >>= \case
            Just ch -> removeChild_ node ch *> go (c - 1)
            Nothing -> pure ()
        
      applyAttr :: Attribute msg s t -> JSM (JSM ())
      applyAttr (Attribute setup) = setup store sinkAttr el

      modifyAt :: forall a. Int -> (a -> a) -> [a] -> [a]
      modifyAt _ _ [] = []
      modifyAt idx f (x:xs)
        | idx == 0 = f x : xs
        | otherwise = x : modifyAt (idx - 1) f xs
        
      itemMsg
        :: Int
        -> UIMsg ui (Int -> msg) a b
        -> UIMsg ui msg s t
      itemMsg _  (Ref ui) = Ref ui
      itemMsg idx (Step f) = Step \s -> s & stbb %~ modifyAt idx (f . props s)
      itemMsg idx (Yield f) = Yield $ f idx

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
                 s <- readStore store
                 let defaultA = props s $ new !! (length old + idx)
                 let projMaybeA s = props s <$> nth idx (getConst (stbb Const s))
                 let itemStore = Store.mapMaybe defaultA projMaybeA store
                 UIHandle node finalizer <- unUI child itemStore $ sink . itemMsg idx
                 appendChild_ el node
                 liftIO $ modifyIORef itemFinalizers (finalizer :)
           | length new < length old -> do
               let d = length old - length new
               dropped <- liftIO $ atomicModifyIORef' itemFinalizers (\xs -> (drop d xs, take d xs))
               sequence_ dropped
               removeLastNChildren d (toNode el)
           | otherwise -> pure ()
        
  unsubscribe <- updates store `subscribe` \Updates {new, old} ->
    setup (getConst (stbb Const old)) (getConst (stbb Const new))
  attrFinalizers <- for attrs applyAttr
  setup [] alist
  pure $ UIHandle (toNode el) (childFinalizer *> sequence_ attrFinalizers *> unsubscribe)

-- ** Element properties

propDyn :: ToJSVal val => Text -> (i -> val) -> Attribute msg i o
propDyn name f = Attribute \store _ el -> do
  model <- readStore store
  jsprop <- toJSVal (f model)
  setProp (toJSString name) jsprop (unsafeCoerce el)
  finalizer <- updates store `subscribe` \Updates {new} -> do
    newProp <- toJSVal (f new)
    setProp (toJSString name) newProp (unsafeCoerce el)
  pure finalizer

prop :: ToJSVal val => Text -> val -> Attribute msg i o
prop name val = Attribute \_ _ el -> do
  jsval <- toJSVal val
  setProp (toJSString name) jsval (unsafeCoerce el)
  pure $ pure ()

attr :: Text -> Text -> Attribute msg i o
attr name val = Attribute \_ _ el -> do
  let self = uncheckedCastTo HTMLElement el
  setAttribute self name val
  pure $ pure ()
  
attrDyn :: Text -> (i -> Text) -> Attribute msg i o
attrDyn name f = Attribute \store _ el -> do
  let self = uncheckedCastTo HTMLElement el
  model <- readStore store 
  setAttribute self name (f model)
  finalizer <- updates store `subscribe` \Updates {new} -> do
    setAttribute self name (f new)
  pure finalizer

-- ** Attaching events to elements

on
  :: IsEvent e
  => EventName HTMLElement e
  -> (i -> Either msg (i -> o))
  -> Attribute msg i o
on eventName makeMsg = Attribute setup
  where
    setup model sink el = E.on el eventName $ liftJSM $ readStore model >>= sink . makeMsg

on_
  :: IsEvent e
  => EventName HTMLElement e
  -> msg
  -> Attribute msg i o
on_ eventName msg = on eventName (const . Left $ msg)

onWithOptions
  :: (IsEvent e, ToJSVal e)
  => EventName HTMLElement e
  -> Decoder a
  -> (i -> a -> Either msg (i -> o))
  -> Attribute msg i o
onWithOptions eventName decoder makeMsg = Attribute setup
  where
    setup store sink el = E.on el eventName do
      event <- ask >>= liftJSM . toJSVal
      model <- liftJSM $ readStore store
      result <- liftJSM (runDecoder decoder event)
      case result of
        Right a -> liftJSM $ sink $ makeMsg model a
        Left err -> liftIO $ putStrLn err

onWithOptions_
  :: (IsEvent e, ToJSVal e)
  => EventName HTMLElement e
  -> Decoder a
  -> (a -> msg)
  -> Attribute msg i o
onWithOptions_ eventName decoder makeMsg
  = onWithOptions eventName decoder (\_ -> Left . makeMsg)

onInput :: (i -> Text -> Either msg (i -> o)) -> Attribute msg i o
onInput = onWithOptions E.input valueDecoder

onInput_ :: (Text -> msg) -> Attribute msg i o
onInput_ makeMsg = onWithOptions E.input valueDecoder \_ -> Left . makeMsg

-- ** Some synomyms for 'prop' and 'propDyn'

boolPropDyn :: Text -> (i -> Bool) -> Attribute msg i o
boolPropDyn = propDyn

boolProp :: Text -> Bool -> Attribute msg i o
boolProp = prop

stringPropDyn :: Text -> (i -> Text) -> Attribute msg i o
stringPropDyn = propDyn

stringProp :: Text -> Text -> Attribute msg i o
stringProp = prop

textPropDyn :: Text -> (i -> Text) -> Attribute msg i o
textPropDyn = propDyn

textProp :: Text -> Text -> Attribute msg i o
textProp = prop

intProp :: Text -> Int -> Attribute msg i o
intProp = prop

intPropDyn :: Text -> (i -> Int) -> Attribute msg i o
intPropDyn = propDyn

doublePropDyn ::  Text -> (i -> Double) -> Attribute msg i o
doublePropDyn = propDyn

doubleProp ::  Text -> Double -> Attribute msg i o
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
  :: Node
  -> Config msg model
  -> JSM ()
attachToNode parent (Config init view eval setup _) = do
  model <- init
  storeHandle <- createStore model
  appHandleRef <- liftIO $ newIORef Nothing
  nodeRef <- liftIO $ newIORef Nothing
  let sink (Yield msg) = liftIO (readIORef appHandleRef) >>= \case
        Just appHandle -> eval appHandle msg
        Nothing        -> pure ()
      sink (Step io) = modifyStore storeHandle io
      sink (Ref newNode) = liftIO (readIORef nodeRef) >>= \case
        Just oldNode -> do
          liftIO $ writeIORef nodeRef (Just newNode)
          DOM.replaceChild_ parent newNode oldNode
        Nothing      -> pure ()
  UIHandle node finalizer <- unUI view (getStore storeHandle) sink
  let appHandle = AppHandle (readStore $ getStore storeHandle) (sink . Step) (sink . Yield) finalizer
  liftIO $ writeIORef appHandleRef (Just appHandle)
  liftIO $ writeIORef nodeRef (Just node)
  DOM.appendChild_ parent node
  setup appHandle
  
attachToBody config = do
  doc <- DOM.currentDocumentUnchecked
  body <- DOM.getBodyUnchecked doc
  attachToNode (DOM.toNode body) config

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
