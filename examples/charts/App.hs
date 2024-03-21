module App where

import Charts qualified as Charts
import Control.Monad.State
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import Sketch
import Sketch.FFI


data AppState = AppState
  { charts_instance :: Charts.ChartState
  } deriving (Show, Eq, Generic)

new :: IO AppState
new = do
  charts_instance <- Charts.new
  return AppState
    { charts_instance
    }

html :: BuilderM AppState ()
html = do
  el "style" [] $ text styles
  embedFld @"charts_instance" Charts.html

styles :: Text
styles = "\
  \ \
  \ "
