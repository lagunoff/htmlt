module JsMain where

import Data.Maybe
import Clickable
import Clickable.Protocol

import "this" TodoList qualified as TodoList
import "this" Utils

jsMain :: StartFlags -> ClickM ()
jsMain _ = do
  items <- fromMaybe [] <$> readLocalStorage "todo-items"
  todoListStateRef <- TodoList.eval $ TodoList.InitAction items
  installFinalizer do
    tolistState <- readVar todoListStateRef
    saveLocalStorage "todo-items" tolistState.items
  attachToBody do
    el "style" $ text TodoList.styles
    TodoList.html TodoList.TodoListConfig
      { state_ref = todoListStateRef
      }
    -- globalAddEventListener (Id "window") popstateEventArgs \loc -> do
    --   let filter = fromMaybe TodoList.All $ TodoList.parseFilter loc.hash
    --   modifyRef todoListStateRef \s -> s {TodoList.filter}
