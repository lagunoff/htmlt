{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (void)
import Massaraksh.Component
import Polysemy
import qualified TodoMVC.Todos as Todos
import qualified TodoMVC.Utils as Utils

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
