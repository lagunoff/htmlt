{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.DOM where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Coerce
import Data.Default
import GHC.Generics
import Data.IORef
import Language.Javascript.JSaddle as JS

import Massaraksh.Decode
import Massaraksh.IdSupply

newtype Node = Node {unNode :: JSVal}
  deriving newtype (MakeArgs, MakeObject, ToJSVal)

data Node1
  = JSNode { jsnode_value :: JSVal }
  | Fragment
    { fragment_id :: Id Node1
    , fragment_root :: IORef (Maybe Node1)
    , fragment_children :: IORef [Node1] }

data ListenOpts = ListenOpts
  { stopPropagation :: Bool
  , preventDefault  :: Bool }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal)

instance Default ListenOpts where
  def = ListenOpts True False

appendChild1 :: Node1 -> Node1 -> JSM ()
appendChild1 root child = case (root, child) of
  (JSNode jsRoot, JSNode jsChild) ->
    void (jsRoot # ("appendChild"::Text) $ jsChild)
  (Fragment{..}, JSNode{}) ->
    liftIO $ modifyIORef fragment_children (<> [child])
  (JSNode jsPar, Fragment{..}) -> do
    liftIO $ writeIORef fragment_root (Just root)
    let
      go = mapM_ \case
        JSNode n -> void (jsPar # ("appendChild"::Text) $ n)
        Fragment{..} -> liftIO (readIORef fragment_children) >>= go
    liftIO (readIORef fragment_children) >>= go
  ((Fragment _ _ ch1), (Fragment _ r2 _)) -> do
    liftIO $ modifyIORef ch1 (<> [child])
    liftIO $ writeIORef r2 (Just root)

removeChild1 :: Node1 -> Node1 -> JSM ()
removeChild1 parent child = case (parent, child) of
  (JSNode jsPar, JSNode jsChild) ->
    void (jsPar # ("removeChild"::Text) $ jsChild)
  (Fragment{..}, JSNode jsChild) -> do
    childs <- liftIO $ readIORef fragment_children
    newChilds <- flip filterM childs \case
      JSNode jsVal -> not <$> strictEqual jsChild jsVal
      _ -> return True
    liftIO $ writeIORef fragment_children newChilds
  (JSNode jsPar, Fragment{..}) -> do
    liftIO $ writeIORef fragment_root Nothing
    let
      go = mapM_ \case
        JSNode n -> void (jsPar # ("removeChild"::Text) $ n)
        Fragment{..} -> liftIO (readIORef fragment_children) >>= go
    liftIO (readIORef fragment_children) >>= go
  ((Fragment _ p1 ch1), (Fragment id2 r2 _)) -> do
    let
      f = \case Fragment{..} -> fragment_id /= id2; _ -> True
      go = \case
        Just (JSNode jsVal) -> removeChild1 (JSNode jsVal) child
        Just Fragment{..} -> liftIO (readIORef fragment_root) >>= go
        Nothing -> return ()
    liftIO $ modifyIORef ch1 (Prelude.filter f)
    liftIO (readIORef p1) >>= go

setAttribute1 :: Node1 -> Text -> Text -> JSM ()
setAttribute1 n k v = case n of
  JSNode jsNode ->
    void $ jsNode # ("setAttribute"::Text) $ (k, v)
  Fragment{} ->
    return ()

removeAttribute1 :: Node1 -> Text -> JSM ()
removeAttribute1 n k = case n of
  JSNode jsNode ->
    void $ jsNode # ("removeAttribute"::Text) $ [k]
  Fragment{} ->
    return ()

#ifndef ghcjs_HOST_OS
appendChild :: Node -> Node -> JSM ()
appendChild root child = do
  void (root # ("appendChild"::Text) $ child)
#else
foreign import javascript unsafe
  "$1.appendChild($2)"
  appendChild :: Node -> Node -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
setAttribute :: Node -> Text -> Text -> JSM ()
setAttribute e k v = do
  void $ e # ("setAttribute"::Text) $ (k, v)
#else
foreign import javascript unsafe
  "$1.setAttribute($2, $3)"
  setAttribute :: Node -> Text -> Text -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
removeAttribute :: Node -> Text -> JSM ()
removeAttribute e k = do
  void $ e # ("removeAttribute"::Text) $ [k]
#else
foreign import javascript unsafe
  "$1.removeAttribute($2)"
  removeAttribute :: Node -> Text -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
removeChild :: Node -> Node -> JSM ()
removeChild p ch = do
  void $ p # ("removeChild"::Text) $ [ch]
#else
foreign import javascript unsafe
  "$1.removeChild($2)"
  removeChild :: Node -> Node -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
replaceChild :: Node -> Node -> Node -> JSM ()
replaceChild root new old = do
  void (root # ("replaceChild"::Text) $ (new, old))
#else
foreign import javascript unsafe
  "$1.replaceChild($2, $3)"
  replaceChild :: Node -> Node -> Node -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
childLength :: Node -> JSM Int
childLength e = do
  fromJSValUnchecked =<< e ! ("childNodes"::Text) ! ("length"::Text)
#else
foreign import javascript unsafe
  "$1.childNodes.length"
  childLength :: Node -> JSM Int
#endif

#ifndef ghcjs_HOST_OS
getChildNode :: Node -> Int -> JSM Node
getChildNode e ix =
  fmap coerce (e ! ("childNodes"::Text) JS.!! ix)
#else
foreign import javascript unsafe
  "$1.childNodes[$2]"
  getChildNode :: Node -> Int -> JSM Node
#endif

#ifndef ghcjs_HOST_OS
createElement :: Text -> JSM Node
createElement tag = do
  fmap coerce $ jsg ("document"::Text) # ("createElement"::Text) $ [tag]
#else
foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: Text -> JSM Node
#endif

#ifndef ghcjs_HOST_OS
createElementNS :: Text -> Text -> JSM Node
createElementNS ns tag = do
  fmap coerce $ jsg ("document"::Text) # ("createElementNS"::Text) $ [ns, tag]
#else
foreign import javascript unsafe
  "document.createElementNS($1, $2)"
  createElementNS :: Text -> Text -> JSM Node
#endif

#ifndef ghcjs_HOST_OS
createTextNode :: Text -> JSM Node
createTextNode tag = do
  fmap coerce $ jsg ("document"::Text) # ("createTextNode"::Text) $ [tag]
#else
foreign import javascript unsafe
  "document.createTextNode($1)"
  createTextNode :: Text -> JSM Node
#endif

#ifndef ghcjs_HOST_OS
classListAdd :: Node -> Text -> JSM ()
classListAdd e c = do
  void $ e ! ("classList"::Text) # ("add"::Text) $ [c]
#else
foreign import javascript unsafe
  "$1.classList.add($2)"
  classListAdd :: Node -> Text -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
classListRemove :: Node -> Text -> JSM ()
classListRemove e c = do
  void $ e ! ("classList"::Text) # ("remove"::Text) $ [c]
#else
foreign import javascript unsafe
  "$1.classList.remove($2)"
  classListRemove :: Node -> Text -> JSM ()
#endif

#ifndef ghcjs_HOST_OS
setTextValue :: Node -> Text -> JSM ()
setTextValue e c = do
  void $ e <# ("nodeValue"::Text) $ c
#else
foreign import javascript unsafe
  "$1.nodeValue = $2;"
  setTextValue :: Node -> Text -> JSM ()
#endif

addEventListener
  :: ListenOpts -> Node -> Text -> (JSVal -> JSM ()) -> JSM (JSM ())
addEventListener ListenOpts{..} target name f = do
  cb <- function \_ _ [event] -> do
    when stopPropagation do
      void $ event # ("stopPropagation"::Text) $ ()
    when preventDefault do
      void $ event # ("preventDefault"::Text) $ ()
    f event
  target # ("addEventListener"::Text) $ (name, cb)
  pure do
    target # ("removeEventListener"::Text) $ (name, cb)
    freeFunction cb

decodeTarget :: Decoder JSVal
decodeTarget = decodeAt ["target"] decodeJSVal

decodeValue :: Decoder Text
decodeValue = decodeAt ["target", "value"] decoder

decodeCurrentTarget :: Decoder JSVal
decodeCurrentTarget = decodeAt ["currentTarget"] decodeJSVal

decodeChecked :: Decoder Bool
decodeChecked = decodeAt ["target", "checked"] decoder

data DeltaMouse = DeltaMouse
  { deltaX :: Int
  , deltaY :: Int
  , deltaZ :: Int }
  deriving stock (Eq, Show, Generic)

decodeDeltaMouse :: Decoder DeltaMouse
decodeDeltaMouse = DeltaMouse
  <$> decodeAt ["deltaX"] decoder
  <*> decodeAt ["deltaY"] decoder
  <*> decodeAt ["deltaZ"] decoder

data Position = Position
  { x :: Int
  , y :: Int }
  deriving stock (Eq, Show, Ord, Generic)

decodeClientXY :: Decoder Position
decodeClientXY = Position
  <$> decodeAt ["clientX"] decoder
  <*> decodeAt ["clientY"] decoder

decodeOffsetXY :: Decoder Position
decodeOffsetXY = Position
  <$> decodeAt ["offsetX"] decoder
  <*> decodeAt ["offsetY"] decoder

decodePageXY :: Decoder Position
decodePageXY = Position
  <$> decodeAt ["pageX"] decoder
  <*> decodeAt ["pageY"] decoder

data Keys = Keys
  { altKey :: Bool
  , ctrlKey :: Bool
  , metaKey :: Bool
  , shiftKey :: Bool
  }
  deriving stock (Eq, Show, Generic)

decodeKeys :: Decoder Keys
decodeKeys = Keys
  <$> decodeAt ["altKey"] decoder
  <*> decodeAt ["ctrlKey"] decoder
  <*> decodeAt ["metaKey"] decoder
  <*> decodeAt ["shiftKey"] decoder

decodeKeyCode :: Decoder Int
decodeKeyCode = decodeAt ["keyCode"] decoder

data KeyboardEvent = KeyboardEvent
  { keys :: Keys
  , key :: Maybe Text
  , keyCode :: Int
  , repeat :: Bool
  }
  deriving stock (Eq, Show, Generic)

keyboardEvent :: Decoder KeyboardEvent
keyboardEvent = KeyboardEvent
  <$> decodeKeys
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder

getCurrentWindow :: MonadJSM m => m JSVal
getCurrentWindow = liftJSM (jsg ("window"::Text))

getDocument :: MonadJSM m => JSVal -> m JSVal
getDocument self = liftJSM (self ! ("document"::Text))

getCurrentBody :: MonadJSM m => m Node
getCurrentBody = liftJSM (Node <$> jsg ("document"::Text) ! ("body"::Text))
