module App where

import Control.Monad.Trans
import Data.Maybe
import Clickable

import "this" TodoList qualified as TodoList
import "this" Utils

app :: StartFlags -> ClickM ()
app _ = do
  items <- fromMaybe [] <$> readLocalStorage "todo-items"
  stateVar <- TodoList.new items
  installFinalizer do
    s <- readVar stateVar
    saveLocalStorage "todo-items" s.items
  attachToBody do
    el "style" $ text TodoList.styles
    TodoList.html TodoList.TodoListConfig
      { state_var = stateVar
      }
    lift $ addEventListener popstateEvent \loc -> do
      let filter = fromMaybe TodoList.All $ TodoList.parseFilter loc.hash
      modifyVar_ stateVar \s -> s {TodoList.filter}
