import Control.Monad.Reader
import Clickable
import GHC.Ptr
import GHC.Word
import App (app)

main :: IO ()
main = return ()

foreign export ccall wasm_main :: Ptr Word8 -> IO ()
wasm_main p = do
  _ <- runApp p app
  return ()
