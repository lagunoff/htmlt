module Charts where

import Data.Proxy
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics

import Clickable
import Clickable.FFI
import PairSelector qualified as PairSelector

-------------------------------------------------------
-- EXAMPLE OF APPLICATION FEATURING FINANCIAL CHARTS --
-------------------------------------------------------

data ChartState = ChartState
  { main_transform :: Transform2D
  , mouse_position :: Point
  , counter :: Int
  } deriving (Show, Eq, Generic)

data ChartInstance = ChartInstance
  { state_var :: DynVar ChartState
  , pair_selector_instance :: DynVar PairSelector.PairSelectorState
  } deriving (Generic)

new :: ClickM ChartInstance
new = do
  pair_selector_instance <- PairSelector.new
  state_var <- newVar ChartState
    { main_transform = Transform2D 0 0 0 0
    , mouse_position = Point 0 0
    , counter = 0
    }
  return ChartInstance {state_var, pair_selector_instance}

html :: ChartInstance -> HtmlM ()
html self = do
  el "style" $ text styles
  div_ [class_ "Charts-root"] do
    h1_ $ text "Canvas with candle chart"
    div_ do
      PairSelector.html self.pair_selector_instance
    div_ do
      button_ do
        text "Clickable this button"
        on @"click" do
          modifyVar self.state_var \s -> s {counter = s.counter + 1 }
      button_ do
        text "Print state"
        on @"click" do
          s <- readVar self.state_var
          consoleLog $ Text.pack $ show s
      span_ [] $ dynText $ self.state_var `mapVar` \s ->
        "You clicked " <> Text.pack (show s.counter) <> " times"
    canvas_ [class_ "Charts-canvas"] $ return ()

styles :: Text
styles = "\
  \.Charts-root {\
  \  max-width: 900;\
  \  width: 100%\
  \  padding: 16px;\
  \  margin: 0 auto;\
  \}\
  \.Charts-canvas {\
  \  width: 100%;\
  \  height: 550px;\
  \  border: solid 1px black;\
  \}\
  \ "

data Transform2D = Transform2D
  { a :: Double -- ^ X scaling
  , c :: Double -- ^ X translation
  , e :: Double -- ^ Y scaling
  , f :: Double -- ^ Y translation
  } deriving (Show, Eq, Generic)

data Point = Point
  { point_x :: Double
  , point_y :: Double
  } deriving (Eq, Show, Generic)

-- setMouseCoords :: Point -> Edit ChartState
-- setMouseCoords p = Fld (Proxy @"mouse_position") (Ins p)


-- moveScreen :: Double -> Jet ChartState -> Jet ChartState
-- moveScreen dir old =
--   let
--     beg = negate old.position.main_transform.c / old.position.main_transform.a
--     end = (w - old.position.main_transform.c) / old.position.main_transform.a
--     inc = (end - beg) * dir
--     w = 1000
--     c = old.position.main_transform.c -
--       (inc * old.position.main_transform.a)
--     new = old.position
--       { main_transform = old.position.main_transform {c}
--       }
--   in
--     Jet new (Fld (Proxy @"main_transform") (Fld (Proxy @"c") (Ins c)) : old.velocity)
