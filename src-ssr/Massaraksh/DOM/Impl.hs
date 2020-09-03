{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.DOM.Impl where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.HashTable.IO as HT
import Data.IORef
import Data.Foldable
import Data.List as L
import Data.Text as T
import Data.Typeable
import GHC.Generics
import Language.Javascript.JSaddle as JS

data Node
  = SsrElement
    { ssreNs     :: Maybe Text
    , ssreTag    :: Text
    , ssreAttrs  :: CuckooHashTable Text Text
    , ssreChilds :: IORef [Node] }
  | SsrText (IORef Text)
  | JsNode JSVal

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
appendChild (JsNode p) (JsNode c) =
  void $ p # ("appendChild"::Text) $ c
appendChild _ _ = pure ()

setAttribute :: Node -> Text -> Text -> JSM ()
setAttribute (SsrElement _ _ attRef _) k v =
  liftIO $ HT.insert attRef k v
setAttribute (JsNode e) k v =
  void $ e # ("setAttribute"::Text) $ (k, v)
setAttribute _ _ _ = pure ()

setProp
  :: (ToJSVal v, Typeable v) => Node -> Text -> v -> JSM ()
setProp (SsrElement _ _ attRef _) k v
  | Just t <- cast @_ @Text v = liftIO $
    for_ (propToAtt k) \k' -> HT.insert attRef k' t
  | Just s <- cast @_ @String v = liftIO $
    for_ (propToAtt k) \k' -> HT.insert attRef k' (T.pack s)
  | otherwise = pure ()
setProp (JsNode e) k v =
  void $ e <# k $ v
setProp _ _ _ = pure ()

propToAtt :: Text -> Maybe Text
propToAtt = \case
  "className" -> Just "class"
  x           -> Just x

removeAttribute :: Node -> Text -> JSM ()
removeAttribute (SsrElement _ _ attRef _) k = do
  liftIO $ HT.delete attRef k
removeAttribute (JsNode e) k = do
  void $ e # ("removeAttribute"::Text) $ [k]
removeAttribute  _ _ = pure ()

removeChild :: Node -> Node -> JSM ()
removeChild (JsNode p) (JsNode c) =
  void $ p # ("removeChild"::Text) $ [c]
removeChild _ _ = pure ()

replaceChild :: Node -> Node -> Node -> JSM ()
replaceChild (JsNode p) (JsNode new) (JsNode old) =
  void $ p # ("replaceChild"::Text) $ (new, old)
replaceChild _ _ _ = pure ()

childLength :: Node -> JSM Int
childLength (SsrElement _ _ _ chRef) =
  liftIO $ L.length <$> readIORef chRef
childLength (JsNode e) =
  fromJSValUnchecked =<< e ! ("childNodes"::Text) ! ("length"::Text)
childLength _ = pure 0

isText :: Node -> JSM Bool
isText (JsNode e) = e `instanceOf` jsg ("Text"::Text)
isText (SsrText _) = pure True
isText (SsrElement _ _ _ _) = pure False

getChildNode :: Node -> Int -> JSM (Maybe Node)
getChildNode (SsrElement _ _ _ chRef) ix =
  liftIO $ elemAt ix <$> readIORef chRef
  where
    elemAt _ []     = Nothing
    elemAt 0 (x:_)  = Just x
    elemAt n (_:xs) = elemAt (n - 1) xs
getChildNode (JsNode e) ix =
  fmap (fmap JsNode) . fromJSValUnchecked =<< (e ! ("childNodes"::Text) JS.!! ix)
getChildNode _ _ = error "getChildNode called on SsrText"

createElement :: Node -> Text -> JSM Node
createElement SsrElement{} t = liftIO do
  att <- HT.new
  ch <- newIORef []
  pure $ SsrElement Nothing t att ch
createElement JsNode{} t =
  fmap JsNode $ jsg ("document"::Text) # ("createElement"::Text) $ [t]
createElement _ _ = error "createElement called on SsrText"

createElementNS :: Node -> Text -> Text -> JSM Node
createElementNS SsrElement{} ns t = liftIO do
  att <- HT.new
  ch <- newIORef []
  pure $ SsrElement (Just ns) t att ch
createElementNS JsNode{} ns t =
  fmap JsNode $ jsg ("document"::Text) # ("createElementNS"::Text) $ [ns, t]
createElementNS _ _ _ = error "createElementNS called on SsrText"

createTextNode :: Node -> Text -> JSM Node
createTextNode SsrElement{} t = liftIO do
  ref <- newIORef t
  pure (SsrText ref)
createTextNode JsNode{} t =
  fmap JsNode $ jsg ("document"::Text) # ("createTextNode"::Text) $ [t]
createTextNode _ _ = error "createTextNode called on SsrText"

classListAdd :: Node -> Text -> JSM ()
classListAdd (JsNode e) c =
  void $ e ! ("classList"::Text) # ("add"::Text) $ [c]
classListAdd _ _ = pure ()

classListRemove :: Node -> Text -> JSM ()
classListRemove (JsNode e) c =
  void $ e ! ("classList"::Text) # ("remove"::Text) $ [c]
classListRemove _ _ = pure ()

setTextValue :: Node -> Text -> JSM ()
setTextValue (SsrText ref) t =
  liftIO $ writeIORef ref t
setTextValue (JsNode e) c =
  void $ e <# ("nodeValue"::Text) $ c
setTextValue _ _ = pure ()

addEventListener
  :: ListenOpts -> Node -> Text -> (JSVal -> JSM ()) -> JSM (JSM ())
addEventListener ListenOpts{..} (JsNode t) k f = do
  cb <- function \_ _ [event] -> do
    when stopPropagation do
      void $ event # ("stopPropagation"::Text) $ ()
    when preventDefault do
      void $ event # ("preventDefault"::Text) $ ()
    f event
  t # ("addEventListener"::Text) $ (k, cb)
  pure do
    t # ("removeEventListener"::Text) $ (k, cb)
    freeFunction cb
addEventListener _ _ _ _ = pure (pure ())
