{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
module App where

import Data.Maybe
import Clickable

import TodoList qualified as TodoList
import Utils
import TodoItem (TodoItemState(TodoItemState))

app :: StartFlags -> JSM ()
app _ = do
  items <- fromMaybe [] <$> readLocalStorage "todo-items"
  stateVar <- TodoList.new items
  installFinalizer do
    s <- readVar stateVar
    saveLocalStorage "todo-items" s.items
  execHTMLBody do
    el "style" $ text TodoList.styles
    TodoList.view TodoList.TodoListConfig {self = stateVar}
    liftJSM $ addEventListener popstateEvent \loc -> do
      let f = fromMaybe TodoList.All $ TodoList.parseFilter loc.loc_hash
      modifyVar_ stateVar \s -> s {TodoList.filter = f}


app1 :: StartFlags -> JSM ()
app1 _ = do
  items :: [TodoItemState] <- fromMaybe [] <$> readLocalStorage "todo-items"
  execHTMLBody do
    h1_ "sdfsdfsdfsd"
