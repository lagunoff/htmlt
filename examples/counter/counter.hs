import Control.Monad
import HtmlT
import Text.Read (readMaybe)
import Data.Text qualified as Text
import Data.Function ((&))
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent

app :: Html ()
app = do
  -- First create a 'DynRef
  counterRef <- lift $ newRef @Int 0
  div_ do
    input_ [type_ "number"] do
      -- Show the value inside <input>
      dynProp "value" $ Text.pack . show <$> fromRef counterRef
      -- Parse and update the value on each InputEvent
      on @"input" \inp ->
        readMaybe (Text.unpack inp) & mapM_ (writeRef counterRef)
    br_
    -- Decrease the value on each click
    button_ do
      on @"click" $ modifyRef counterRef pred
      text "-"
    -- Increase the value on each click
    button_ do
      on @"click" $ modifyRef counterRef succ
      text "+"

foreign export ccall wasm_main :: IO ()
wasm_main = void $ attachToBody app

main :: IO ()
main = return ()
