
module Massaraksh.DOM.Text where

import Data.Text
import Data.JSString.Text as JSS
import Massaraksh.Decode
import qualified Massaraksh.DOM as H

value :: Decoder Text
value = fmap JSS.textFromJSString H.value
{-# INLINE value #-}
