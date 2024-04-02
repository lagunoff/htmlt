import Clickable
import Clickable.Main.DevServer
import Control.Monad.Reader

import "this" App qualified as App

main :: IO ()
main = runServer \_ _ -> attachToBody $ lift App.new >>= App.html
