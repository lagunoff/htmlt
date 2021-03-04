module Main where

import HtmlT

import Todo.Todos
import Todo.Utils

main :: IO ()
main = withJSM do
  initial <- initTodos
  stateRef <- newRef initial
  let widget = fix1 (todosWidget stateRef)
  (_, htmlEnv) <- attachToBody (widget RenderTodos)
  setup (runHtmlT htmlEnv) (widget BeforeUnload) (widget . HashChange)
