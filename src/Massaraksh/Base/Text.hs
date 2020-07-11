module Massaraksh.Base.Text where

import Data.JSString.Text as JSS
import Data.Text
import Language.Javascript.JSaddle as JS
import Massaraksh.DOM
import Massaraksh.Decode
import Massaraksh.Event
import Massaraksh.Types
import qualified Massaraksh.Base as H

el :: Text -> Html x -> Html x
el tag = H.el (JSS.textToJSString tag)
{-# INLINE el #-}

elNS :: Maybe Text -> Text -> Html x -> Html x
elNS ns tag = H.elNS (fmap JSS.textToJSString ns) (JSS.textToJSString tag)
{-# INLINE elNS #-}

el' :: Text -> Html x -> Html Element
el' tag = H.el' (JSS.textToJSString tag)
{-# INLINE el' #-}

text :: Text -> Html ()
text = H.text . JSS.textToJSString
{-# INLINE text #-}

dynText :: Dynamic Text -> Html ()
dynText = H.dynText . fmap JSS.textToJSString
{-# INLINE dynText #-}

prop :: ToJSVal v => Text -> v -> Html ()
prop key = H.prop (JSS.textToJSString key)
{-# INLINE prop #-}

(=:) :: Text -> Text -> Html ()
(=:) = prop
{-# INLINE (=:) #-}
infixr 3 =:

dynProp
  :: (ToJSVal v, FromJSVal v, Eq v)
  => Text -> Dynamic v -> Html ()
dynProp key = H.dynProp (JSS.textToJSString key)
{-# INLINE dynProp #-}

(~:)
  :: (ToJSVal v, FromJSVal v, Eq v) => Text -> Dynamic v -> Html ()
(~:) = dynProp
{-# INLINE (~:) #-}
infixr 3 ~:

attr :: Text -> Text -> Html ()
attr key val = H.attr (JSS.textToJSString key) (JSS.textToJSString val)
{-# INLINE attr #-}

dynAttr :: Text -> Dynamic Text -> Html ()
dynAttr key dyn = H.dynAttr (JSS.textToJSString key)
  (fmap JSS.textToJSString dyn)
{-# INLINE dynAttr #-}

on :: Text -> Decoder (Html x) -> Html ()
on name = H.on (JSS.textToJSString name)
{-# INLINE on #-}

on_ :: Text -> Html x -> Html ()
on_ name = H.on_ (JSS.textToJSString name)
{-# INLINE on_ #-}

onEvent :: Element -> Text -> Decoder (Html x) -> Html ()
onEvent elm name = H.onEvent elm (JSS.textToJSString name)
{-# INLINE onEvent #-}

onEvent_ :: Element -> Text -> Html x -> Html ()
onEvent_ elm name = H.onEvent_ elm (JSS.textToJSString name)
{-# INLINE onEvent_ #-}

dynClassList :: [(Text, Dynamic Bool)] -> Html ()
dynClassList = H.dynClassList . fmap \(k, v) -> (JSS.textToJSString k, v)
{-# INLINE dynClassList #-}

classList :: [(Text, Bool)] -> Html ()
classList = H.classList . fmap \(k, v) -> (JSS.textToJSString k, v)
{-# INLINE classList #-}
