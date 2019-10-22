{-# LANGUAGE OverloadedStrings #-}
module Main where

import Massaraksh.Html
import qualified Massaraksh.Html.Dynamic as Dyn

view :: Html msg model model
view = div_ [] [ h1_ [] [ text "It works" ] ] 

main = defaultMain view ()
