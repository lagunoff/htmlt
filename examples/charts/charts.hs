import HtmlT.Sketch
import "this" Charts qualified as Charts

main :: IO ()
main = return ()

foreign export ccall wasm_main :: IO ()
wasm_main = Charts.new >>= flip attach Charts.html
