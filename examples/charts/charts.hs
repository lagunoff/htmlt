import Sketch
import "this" App qualified as App

main :: IO ()
main = return ()

foreign export ccall wasm_main :: IO ()
wasm_main = App.new >>= flip attach App.html
