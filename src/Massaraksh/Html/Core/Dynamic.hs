{-# LANGUAGE OverloadedStrings #-}
module Massaraksh.Html.Core.Dynamic where

import Data.Text (Text)
import Massaraksh.Core (UI(..), UIHandle(..))
import Massaraksh.Event (Event(..))
import Massaraksh.Html.Core (Attribute(..), Html, el)
import Massaraksh.Store (Updates(..), Store(..))
import qualified Data.JSString.Text as JSS
import qualified Data.Text.Lazy as LT
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Node as DOM
import GHCJS.DOM.Types (HTMLElement(..), ToJSVal(..), uncheckedCastTo, ToJSString(..))
import Unsafe.Coerce (unsafeCoerce)
import Language.Javascript.JSaddle (setProp)
import qualified GHCJS.DOM.Element as DOM

prop :: ToJSVal val => Text -> (i -> val) -> Attribute msg i o
prop name f = Attribute \store _ el -> do
  model <- readStore store
  jsprop <- toJSVal (f model)
  setProp (toJSString name) jsprop (unsafeCoerce el)
  finalizer <- updates store `subscribe` \Updates {new} -> do
    newProp <- toJSVal (f new)
    setProp (toJSString name) newProp (unsafeCoerce el)
  pure finalizer
  
attr :: Text -> (i -> Text) -> Attribute msg i o
attr name f = Attribute \store _ el -> do
  let self = uncheckedCastTo HTMLElement el
  model <- readStore store 
  DOM.setAttribute self name (f model)
  finalizer <- updates store `subscribe` \Updates {new} -> do
    DOM.setAttribute self name (f new)
  pure finalizer

boolProp :: Text -> (i -> Bool) -> Attribute msg i o
boolProp = prop

stringProp :: Text -> (i -> String) -> Attribute msg i o
stringProp = prop

textProp :: Text -> (i -> Text) -> Attribute msg i o
textProp = prop

intProp :: Text -> (i -> Int) -> Attribute msg i o
intProp = prop

doubleProp ::  Text -> (i -> Double) -> Attribute msg i o
doubleProp = prop

unsafeHtml :: Text -> [Attribute msg i o] -> (i -> Text) -> Html msg i o
unsafeHtml name other html =
  el name (prop "innerHTML" html:other) []

text :: (i -> Text) -> Html msg i o
text f = UI \model _ -> do
  doc <- DOM.currentDocumentUnchecked
  content <- f <$> readStore model
  ui <- DOM.toNode <$> DOM.createTextNode doc (JSS.textToJSString content)
  unsubscribe <- updates model `subscribe` \Updates {new} ->
    DOM.setTextContent ui (Just (f new))
  pure (UIHandle ui unsubscribe)

lazyText :: (i -> LT.Text) -> Html msg i o
lazyText f = UI \model _ -> do
  doc <- DOM.currentDocumentUnchecked
  content <- f <$> readStore model
  ui <- DOM.toNode <$> DOM.createTextNode doc (JSS.textToJSString $ LT.toStrict content)
  unsubscribe <- updates model `subscribe` \Updates {new} ->
    DOM.setTextContent ui (Just (LT.toStrict . f $ new))
  pure (UIHandle ui unsubscribe)
