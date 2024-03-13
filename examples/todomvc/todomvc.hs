import Control.Monad
import Control.Monad.Reader
import HtmlT

import "this" TodoList qualified as TodoList
import "this" Utils

main :: IO ()
main = return ()

foreign export ccall wasm_main :: IO ()
wasm_main = void $ attachToBody do
  renv <- asks (.html_reactive_env)
  urlHashRef <- mkUrlHashRef
  todosRef <- dynStep $ TodoList.eval (TodoList.InitAction renv urlHashRef)
  TodoList.html $ TodoList.TodoListConfig todosRef
