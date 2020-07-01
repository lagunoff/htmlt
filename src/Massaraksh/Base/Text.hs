module Massaraksh.Base.Text where

import Data.JSString.Text as JSS
import Data.Text
import Language.Javascript.JSaddle as JS
import Massaraksh.DOM
import Massaraksh.Decode
import Massaraksh.Event
import Massaraksh.Types
import qualified Massaraksh.Base as H

el :: HtmlBase m => Text -> HtmlT m x -> HtmlT m x
el tag = H.el (JSS.textToJSString tag)
{-# INLINE el #-}

elNS :: HtmlBase m => Maybe Text -> Text -> HtmlT m x -> HtmlT m x
elNS ns tag = H.elNS (fmap JSS.textToJSString ns) (JSS.textToJSString tag)
{-# INLINE elNS #-}

el' :: HtmlBase m => Text -> HtmlT m x -> HtmlT m Element
el' tag = H.el' (JSS.textToJSString tag)
{-# INLINE el' #-}

text :: HtmlBase m => Text -> HtmlT m ()
text = H.text . JSS.textToJSString
{-# INLINE text #-}

dynText :: HtmlBase m => Dynamic Text -> HtmlT m ()
dynText = H.dynText . fmap JSS.textToJSString
{-# INLINE dynText #-}

prop :: (ToJSVal v, HtmlBase m) => Text -> v -> HtmlT m ()
prop key = H.prop (JSS.textToJSString key)
{-# INLINE prop #-}

(=:) :: HtmlBase m => Text -> Text -> HtmlT m ()
(=:) = prop
{-# INLINE (=:) #-}
infixr 3 =:

dynProp
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v)
  => Text -> Dynamic v -> HtmlT m ()
dynProp key = H.dynProp (JSS.textToJSString key)
{-# INLINE dynProp #-}

(~:)
  :: (HtmlBase m, ToJSVal v, FromJSVal v, Eq v) => Text -> Dynamic v -> HtmlT m ()
(~:) = dynProp
{-# INLINE (~:) #-}
infixr 3 ~:

attr :: HtmlBase m => Text -> Text -> HtmlT m ()
attr key val = H.attr (JSS.textToJSString key) (JSS.textToJSString val)
{-# INLINE attr #-}

dynAttr :: HtmlBase m => Text -> Dynamic Text -> HtmlT m ()
dynAttr key dyn = H.dynAttr (JSS.textToJSString key)
  (fmap JSS.textToJSString dyn)
{-# INLINE dynAttr #-}

on :: HtmlBase m => Text -> Decoder (HtmlT m x) -> HtmlT m ()
on name = H.on (JSS.textToJSString name)
{-# INLINE on #-}

on_ :: HtmlBase m => Text -> HtmlT m x -> HtmlT m ()
on_ name = H.on_ (JSS.textToJSString name)
{-# INLINE on_ #-}

onEvent :: HtmlBase m => Element -> Text -> Decoder (HtmlT m x) -> HtmlT m ()
onEvent elm name = H.onEvent elm (JSS.textToJSString name)
{-# INLINE onEvent #-}

onEvent_ :: HtmlBase m => Element -> Text -> HtmlT m x -> HtmlT m ()
onEvent_ elm name = H.onEvent_ elm (JSS.textToJSString name)
{-# INLINE onEvent_ #-}

dynClassList :: HtmlBase m => [(Text, Dynamic Bool)] -> HtmlT m ()
dynClassList = H.dynClassList . fmap \(k, v) -> (JSS.textToJSString k, v)
{-# INLINE dynClassList #-}

classList :: HtmlBase m => [(Text, Bool)] -> HtmlT m ()
classList = H.classList . fmap \(k, v) -> (JSS.textToJSString k, v)
{-# INLINE classList #-}
