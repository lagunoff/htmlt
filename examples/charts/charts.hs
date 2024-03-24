import Control.Monad.Reader
import Clickable
import "this" App qualified as App

main :: IO ()
main = return ()

foreign export ccall wasm_main :: IO ()
wasm_main = attach do
  liftClickM App.new >>= App.html
