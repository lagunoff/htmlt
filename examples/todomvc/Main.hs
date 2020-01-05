module Main where

import Todos
import Massaraksh

main :: IO ()
main = withJSM Nothing $
  attachComponentToBody (Fix . ComponentEnvF) component ()
