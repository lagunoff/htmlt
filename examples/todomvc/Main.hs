module Main where

import Todos
import Utils
import Massaraksh
import Language.Javascript.JSaddle.Types ()

main :: IO ()
main = withJSM do
  initialState <- unsafeInit $ htmlFix todosWidget Init
  attachToBodySimple initialState (htmlFix todosWidget Render)
