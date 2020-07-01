
module Massaraksh.DOM.Text where

import Data.Text
import Data.JSString.Text as JSS
import Massaraksh.Decode
import qualified Massaraksh.DOM as H

dValue :: Decoder Text
dValue = fmap JSS.textFromJSString H.dValue
{-# INLINE dValue #-}
