module Main where

import qualified TodoMVC.Todos as Todos
import Polysemy
import Massaraksh.Component

main :: IO ()
main = defaultMain Todos.init Todos.eval Todos.view runSem
  where
    runSem = runM . io2jsm
