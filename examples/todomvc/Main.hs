module Main where

import Todos
import Utils
import Massaraksh
import Language.Javascript.JSaddle.Types ()

main :: IO ()
main = withJSM do
  let widget = htmlFix todosWidget
  initialState <- unsafeInit (widget Init)
  rs <- attachToBodySimple initialState (widget Render)
  setup (rsEval rs) (widget BeforeUnload) (widget . HashChange)
