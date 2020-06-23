module Main where

import Todos
import Utils
import Massaraksh
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Types ()

main :: IO ()
main = withJSM do
  initial <- Todos.init
  dyn <- liftIO (newDyn initial)
  let widget = fix1 (todosWidget dyn)
  rs <- attachToBodySimple (widget Render)
  setup (rsEval rs) (widget BeforeUnload) (widget . HashChange)
