import Control.Monad.Reader
import Clickable.Main.DevServer
import Clickable
import GHC.Ptr
import GHC.Word
import "this" App qualified as App

main :: IO ()
main = runDebugDefault 8081 \_ _ -> attachToBody $ lift App.new >>= App.html
