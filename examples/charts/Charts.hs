module Charts where

import Data.Proxy
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics

import Sketch
import Sketch.FFI
import PairSelector qualified as PairSelector

-------------------------------------------------------
-- EXAMPLE OF APPLICATION FEATURING FINANCIAL CHARTS --
-------------------------------------------------------

data ChartState = ChartState
  { main_transform :: Transform2D
  , mouse_position :: Point
  , counter :: Int
  , pair_selector_instance :: PairSelector.PairSelectorState
  } deriving (Show, Eq, Generic)

new :: IO ChartState
new = do
  pair_selector_instance <- PairSelector.new
  return ChartState
    { main_transform = Transform2D 0 0 0 0
    , mouse_position = Point 0 0
    , counter = 0
    , pair_selector_instance
    }

html :: BuilderM ChartState ()
html = do
  el "style" [] $ text styles
  div_ [("className", "Charts-root")] do
    h1_ [] $ text "Canvas with candle chart"
    div_ [] do
      embedFld @"pair_selector_instance" PairSelector.html
    div_ [] do
      button_ [] do
        text "Click this button"
        on "click" \_ -> do
          modifyFld @"counter" (+ 1)
      button_ [] do
        text "Print state"
        on "click" \_ -> do
          s <- get
          consoleLog $ Text.pack $ show s.position
      span_ [] $ dynText $ \s -> "You clicked " <> Text.pack (show s.counter) <> " times"
    canvas_ [("className", "Charts-canvas")] $ return ()

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

setMouseCoords :: Point -> Edit ChartState
setMouseCoords p = Fld (Proxy @"mouse_position") (Ins p)


moveScreen :: Double -> Jet ChartState -> Jet ChartState
moveScreen dir old =
  let
    beg = negate old.position.main_transform.c / old.position.main_transform.a
    end = (w - old.position.main_transform.c) / old.position.main_transform.a
    inc = (end - beg) * dir
    w = 1000
    c = old.position.main_transform.c -
      (inc * old.position.main_transform.a)
    new = old.position
      { main_transform = old.position.main_transform {c}
      }
  in
    Jet new (Fld (Proxy @"main_transform") (Fld (Proxy @"c") (Ins c)) : old.velocity)
