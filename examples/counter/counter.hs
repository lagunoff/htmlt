import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Text as T
import Text.Read (readMaybe)
import HtmlT

main :: IO ()
main = void $ attachToBody do
  -- First create a 'DynRef
  counterRef <- newRef @Int 0
  div_ do
    input_ do
      -- Show the value inside <input>
      dynProp "value" $ T.pack . show <$> fromRef counterRef
      -- Parse and update the value on each InputEvent
      on "input" $ decodeEvent intDecoder $ writeRef counterRef
    br_
    -- Decrease the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef pred
      text "Decrease"
    -- Increase the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef succ
      text "Increase"
  where
    intDecoder =
      valueDecoder >=> MaybeT . pure . readMaybe . T.unpack
