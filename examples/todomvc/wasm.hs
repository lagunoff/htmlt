import Clickable.WASM
import GHC.Ptr
import GHC.Word
import TodoList

main :: IO ()
main = return ()

foreign export ccall wasm_app :: Ptr Word8 -> IO (Ptr Word8)
wasm_app = mkWasmApp $ const TodoList.run
