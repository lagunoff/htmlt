module Main where

import Todos
import Massaraksh
import Control.Monad.IO.Unlift
import Language.Javascript.JSaddle.Types ()

main :: IO ()
main = withJSM Nothing do
  UnliftIO{..} <- askUnliftIO
  attachToBody component unliftIO (component Init) (component Render)
