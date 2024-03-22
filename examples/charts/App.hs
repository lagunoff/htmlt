module App where

import Charts qualified as Charts
import Control.Monad.State
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import Clickable
import Clickable.FFI


data AppInstance = AppInstance
  { charts_instance :: Charts.ChartInstance
  } deriving (Generic)

new :: ClickM AppInstance
new = do
  charts_instance <- Charts.new
  return AppInstance
    { charts_instance
    }

html :: AppInstance -> HtmlM ()
html self = do
  el "style" [] $ text styles
  Charts.html self.charts_instance

styles :: Text
styles = "\
  \ \
  \ "
