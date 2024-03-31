module App where

import Charts qualified as Charts
import Control.Monad.State
import Control.Concurrent
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import Clickable
import Clickable.FFI


data AppInstance = AppInstance
  { state_var :: DynVar AppTab
  } deriving (Generic)

data AppTab
  = ChartsTab Charts.ChartInstance
  | HelpTab

new :: ClickM AppInstance
new = do
  charts_instance <- Charts.new
  state_var <- newDynVar $ ChartsTab charts_instance
  return AppInstance
    { state_var
    }

html :: AppInstance -> HtmlM ()
html self = do
  el "style" $ text styles
  div_ do
    button_ do
      text "Open Charts"
      on @"click" do
        inst <- Charts.new
        modifyVar_ self.state_var $ const $ ChartsTab inst
    button_ do
      text "Open Help"
      on @"click" $ modifyVar_ self.state_var $ const HelpTab
    button_ do
      text "Open Modal"
      on @"click" do
        mvar <- liftIO newEmptyMVar
        attachToBody do
          p_ $ text "Content of the modal"
          button_ do
            text "Close"
            on @"click" $ liftIO $ putMVar mvar "dljfhdlsfjh"
        syncPoint
        answer <- liftIO $ takeMVar mvar
        -- consoleLog answer
        return ()
  dyn $ self.state_var `mapVar` \case
    ChartsTab inst -> Charts.html inst
    HelpTab -> p_ $ text
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
      \tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
      \veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
      \commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
      \velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
      \occaecat cupidatat non proident, sunt in culpa qui officia deserunt \
      \mollit anim id est laborum."

styles :: Text
styles = "\
  \ \
  \ "
