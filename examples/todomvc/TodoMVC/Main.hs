{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified TodoMVC.Todos as Todos
import qualified TodoMVC.Utils as Utils
import Polysemy
import Massaraksh.Component
import Control.Monad (void)

main :: IO ()
main = defaultMainWith $ Config {..}
  where
    configInit = runM Todos.init
    configView = Todos.view
    configSetup = Utils.setup (Exists . Todos.HashChange) (Exists Todos.BeforeUnload)
    configPort = Nothing
    configEval handle (Exists msg) = void
       $ runM
       $ runStateAppHandle handle
       $ interpMsg Todos.eval
       $ Todos.eval msg
