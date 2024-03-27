module PairSelector where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Function hiding (on)
import GHC.Generics

import Clickable

data TradingPair = TradingPair
  { base :: Text
  , quote :: Text
  , exchange :: Text
  } deriving (Show, Eq, Generic)

data PairSelectorState = PairSelectorState
  { selected_pair :: Maybe TradingPair
  , options :: [TradingPair]
  } deriving (Show, Eq, Generic)

new :: ClickM (DynVar PairSelectorState)
new =
  newDynVar PairSelectorState
    { selected_pair = Nothing
    , options = []
    }

html :: DynVar PairSelectorState -> HtmlM ()
html self = do
  el "style" $ text styles
  div_ [class_ "PairSelector-root"] do
    button_ do
      text "Select pair"
      on @"click" $ modifyVar self \s -> case s.selected_pair of
        Just _ -> s {selected_pair = Nothing}
        Nothing -> s {selected_pair = Just $ TradingPair "BTC" "USDT" "MEXC"}
    span_ $ dynText $ self `mapVar` \s -> s.selected_pair & maybe
      "Nothing selected"
      (("Selected pair: " <>) . Text.pack . show)

styles :: Text
styles = "\
  \ \
  \ "
