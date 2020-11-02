module Main where

import Todos
import Utils
import Component
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Types ()

main :: IO ()
main = withJSM do
  initial <- Todos.init
  dyn <- liftIO (newDyn initial)
  let widget = fix1 (todosWidget dyn)
  (_, ht) <- attachToBody (widget Render)
  setup (runHtml ht) (widget BeforeUnload) (widget . HashChange)
