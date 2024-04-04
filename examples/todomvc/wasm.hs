import Control.Monad.Reader
import Clickable.Main.WASM
import GHC.Ptr
import GHC.Word
import App (app)

main :: IO ()
main = return ()

foreign export ccall wasm_main :: Ptr Word8 -> IO ()
wasm_main = runApp app
