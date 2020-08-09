{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.DOM where

import Control.Monad.IO.Class
import Data.Default
import Data.HashTable.IO as HT
import Data.IORef
import Data.List as L
import Data.Text as T
import Data.Typeable
import GHC.Generics
import Language.Javascript.JSaddle as JS
import Massaraksh.Decode

data Node
  = SsrElement
    { ssreNs     :: Maybe Text
    , ssreTag    :: Text
    , ssreAttrs  :: CuckooHashTable Text Text
    , ssreChilds :: IORef [Node] }
  | SsrText
    { ssrtContent :: IORef Text }

instance MakeArgs Node where makeArgs = undefined
instance MakeObject Node where makeObject = undefined
instance ToJSVal Node where toJSVal = undefined

data ListenOpts = ListenOpts
  { stopPropagation :: Bool
  , preventDefault  :: Bool }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal)

instance Default ListenOpts where
  def = ListenOpts True False

appendChild :: Node -> Node -> JSM ()
appendChild (SsrElement _ _ _ chRef) new = do
  liftIO $ modifyIORef chRef (<> [new])
appendChild _ _ = pure ()

setAttribute :: Node -> Text -> Text -> JSM ()
setAttribute (SsrElement _ _ attRef _) k v = do
  liftIO $ HT.insert attRef k v
setAttribute _ _ _ = pure ()

setProp :: forall v. (ToJSVal v, Typeable v) => Node -> Text -> v -> JSM ()
setProp (SsrElement _ _ attRef _) k v
  | Just t <- cast @_ @Text v = liftIO $ HT.insert attRef k t
  | Just s <- cast @_ @String v = liftIO $ HT.insert attRef k (T.pack s)
  | otherwise = pure ()
setProp _ _ _ = pure ()

removeAttribute :: Node -> Text -> JSM ()
removeAttribute (SsrElement _ _ attRef _) k = do
  liftIO $ HT.delete attRef k
removeAttribute  _ _ = pure ()

removeChild :: Node -> Node -> JSM ()
removeChild _ _ = pure ()

replaceChild :: Node -> Node -> Node -> JSM ()
replaceChild _ _ _ = pure ()

childLength :: Node -> JSM Int
childLength (SsrElement _ _ _ chRef) =
  liftIO $ L.length <$> readIORef chRef
childLength _ = pure 0

getChildNode :: Node -> Int -> JSM Node
getChildNode (SsrElement _ _ _ chRef) ix =
  liftIO $ (L.!! ix) <$> readIORef chRef
getChildNode _ _ = error "getChildNode called on SsrText"

createElement :: Text -> JSM Node
createElement t = liftIO do
  att <- HT.new
  ch <- newIORef []
  pure $ SsrElement Nothing t att ch

createElementNS :: Text -> Text -> JSM Node
createElementNS ns t = liftIO do
  att <- HT.new
  ch <- newIORef []
  pure $ SsrElement (Just ns) t att ch

createTextNode :: Text -> JSM Node
createTextNode t = liftIO do
  content <- newIORef t
  pure $ SsrText content

classListAdd :: Node -> Text -> JSM ()
classListAdd _ _ = pure ()

classListRemove :: Node -> Text -> JSM ()
classListRemove _ _ = pure ()

setTextValue :: Node -> Text -> JSM ()
setTextValue (SsrText ref) t =
  liftIO $ writeIORef ref t
setTextValue _ _ = pure ()

addEventListener
  :: ListenOpts -> Node -> Text -> (JSVal -> JSM ()) -> JSM (JSM ())
addEventListener _ _ _ _ = pure (pure ())

target :: Decoder JSVal
target = decodeAt ["target"] decodeJSVal

value :: Decoder Text
value = decodeAt ["target", "value"] decoder

currentTarget :: Decoder JSVal
currentTarget = decodeAt ["currentTarget"] decodeJSVal

checked :: Decoder Bool
checked = decodeAt ["target", "checked"] decoder

data DeltaMouse = DeltaMouse
  { deltaX :: Int
  , deltaY :: Int
  , deltaZ :: Int }
  deriving stock (Eq, Show, Generic)

deltaMouse :: Decoder DeltaMouse
deltaMouse = DeltaMouse
  <$> decodeAt ["deltaX"] decoder
  <*> decodeAt ["deltaY"] decoder
  <*> decodeAt ["deltaZ"] decoder

data Position = Position
  { x :: Int
  , y :: Int }
  deriving stock (Eq, Show, Ord, Generic)

clientXY :: Decoder Position
clientXY = Position
  <$> decodeAt ["clientX"] decoder
  <*> decodeAt ["clientY"] decoder

offsetXY :: Decoder Position
offsetXY = Position
  <$> decodeAt ["offsetX"] decoder
  <*> decodeAt ["offsetY"] decoder

pageXY :: Decoder Position
pageXY = Position
  <$> decodeAt ["pageX"] decoder
  <*> decodeAt ["pageX"] decoder

data Keys = Keys
  { altKey   :: Bool
  , ctrlKey  :: Bool
  , metaKey  :: Bool
  , shiftKey :: Bool }
  deriving stock (Eq, Show, Generic)

keys :: Decoder Keys
keys = Keys
  <$> decodeAt ["altKey"] decoder
  <*> decodeAt ["ctrlKey"] decoder
  <*> decodeAt ["metaKey"] decoder
  <*> decodeAt ["shiftKey"] decoder

keyCode :: Decoder Int
keyCode = decodeAt ["keyCode"] decoder

data KeyboardEvent = KeyboardEvent
  { keys_    :: Keys
  , key      :: Maybe Text
  , keyCode_ :: Int
  , repeat   :: Bool }
  deriving stock (Eq, Show, Generic)

keyboardEvent :: Decoder KeyboardEvent
keyboardEvent = KeyboardEvent
  <$> keys
  <*> decodeAt ["key"] decoder
  <*> decodeAt ["keyCode"] decoder
  <*> decodeAt ["repeat"] decoder
