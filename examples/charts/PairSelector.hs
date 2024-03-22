module PairSelector where

import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Function hiding (on)
import GHC.Generics

import Clickable
import Clickable.FFI

data TradingPair = TradingPair
  { base :: Text
  , quote :: Text
  , exchange :: Text
  } deriving (Show, Eq, Generic)

data PairSelectorState = PairSelectorState
  { selected_pair :: Maybe TradingPair
  , options :: [TradingPair]
  } deriving (Show, Eq, Generic)

new :: ClickM (IncVar PairSelectorState)
new =
  newVar PairSelectorState
    { selected_pair = Nothing
    , options = []
    }

html :: IncVar PairSelectorState -> HtmlM ()
html self = do
  el "style" [] $ text styles
  div_ [("className", "PairSelector-root")] do
    button_ [] do
      text "Select pair"
      on "click" \_ -> do
        modifyVar self \s -> case s.selected_pair of
          Just _ -> s {selected_pair = Nothing}
          Nothing -> s {selected_pair = Just $ TradingPair "BTC" "USDT" "MEXC"}
    span_ [] $ dynText $ TipVal self `MapVal` \s -> s.selected_pair & maybe
      "Nothing selected"
      (("Selected pair: " <>) . Text.pack . show)

styles :: Text
styles = "\
  \ \
  \ "
