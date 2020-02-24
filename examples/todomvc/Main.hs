module Main where

import Todos
import Massaraksh
import Language.Javascript.JSaddle.Types ()

main :: IO ()
main = let
  component = htmlFix todosWidget
  in withJSM do attachToBodySimple component (component Init) (component Render)
