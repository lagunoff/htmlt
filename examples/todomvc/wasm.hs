import Control.Monad.Reader
import Clickable.Main.Wasm
import GHC.Ptr
import GHC.Word
import App (app)

main :: IO ()
main = return ()

foreign export ccall wasm_app :: Ptr Word8 -> IO ()
wasm_app = runWasm app
