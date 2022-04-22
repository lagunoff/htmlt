import Control.Monad
import HtmlT

import "this" TodoList
import "this" Utils

main :: IO ()
main = void $ attachToBody do
  urlHashRef <- mkUrlHashRef
  todosRef <- initTodos urlHashRef
  todoListWidget $ TodoListConfig todosRef
