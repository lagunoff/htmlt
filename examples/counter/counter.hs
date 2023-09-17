import Control.Monad
import Control.Monad.Trans.Maybe
import Text.Read (readMaybe)
import HtmlT

app :: Html ()
app = do
  -- First create a 'DynRef
  counterRef <- newRef @Int 0
  div_ do
    input_ [type_ "number"] do
      -- Show the value inside <input>
      dynProp "value" $ fromHSString . show <$> fromRef counterRef
      -- Parse and update the value on each InputEvent
      on "input" $ decodeEvent intDecoder $ writeRef counterRef
    br_
    -- Decrease the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef pred
      text "-"
    -- Increase the value on each click
    button_ do
      on_ "click" $ modifyRef counterRef succ
      text "+"
  where
    intDecoder =
      valueDecoder >=> MaybeT . pure . readMaybe . toHSString

main :: IO ()
main =
  void $ attachToBody app
