module Main where

import Todos
import Massaraksh

main :: IO ()
main = withJSM Nothing $
  attachToBody component () (component Init) (component Render)
