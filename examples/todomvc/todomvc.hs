import Control.Monad
import HtmlT

import "this" TodoList
import "this" Utils

main :: IO ()
main = do
  env <- newReactiveEnv
  urlHashRef <- mkUrlHashRef env
  todosRef <- initTodos env urlHashRef
  void $ attachToBody $ todoListWidget $ TodoListConfig todosRef id
