module PairSelector where

import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Function hiding (on)
import GHC.Generics

import Sketch
import Sketch.FFI

data TradingPair = TradingPair
  { base :: Text
  , quote :: Text
  , exchange :: Text
  } deriving (Show, Eq, Generic)

data PairSelectorState = PairSelectorState
  { selected_pair :: Maybe TradingPair
  , options :: [TradingPair]
  } deriving (Show, Eq, Generic)

new :: IO PairSelectorState
new =
  return PairSelectorState
    { selected_pair = Nothing
    , options = []
    }

html :: BuilderM PairSelectorState ()
html = do
  el "style" [] $ text styles
  div_ [("className", "PairSelector-root")] do
    button_ [] do
      text "Select pair"
      on "click" \_ -> do
        modifyFld @"selected_pair" $ \case
          Just _ -> Nothing
          Nothing -> Just $ TradingPair "BTC" "USDT" "MEXC"
    span_ [] $ dynText \s -> s.selected_pair & maybe
      "Nothing selected"
      (("Selected pair: " <>) . Text.pack . show)

styles :: Text
styles = "\
  \ \
  \ "
