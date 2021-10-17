module Main where

import Control.Monad
import HtmlT

import "this" Todo.Todos
import "this" Todo.Utils

main :: IO ()
main = do
  env <- newReactiveEnv
  urlHashRef <- mkUrlHashRef env
  todosRef <- initTodos env urlHashRef
  void $ attachToBody $ todosWidget $ TodosConfig todosRef id
