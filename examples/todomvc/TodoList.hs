module TodoList where

import Control.Monad.IO.Class
import Data.Foldable
import Data.List qualified as List
import Data.Maybe
import GHC.Generics (Generic)
import HtmlT
import JavaScript.Compat.Marshal
import JavaScript.Compat.String (JSString(..))
import JavaScript.Compat.String qualified as JSS

import "this" TodoItem qualified as TodoItem
import "this" Utils

data TodoListConfig = TodoListConfig
  { state_ref :: DynRef TodoListState
  }

data TodoListState = TodoListState
  { title :: JSString
  , items :: [TodoItem.TodoItemState]
  , filter :: Filter
  } deriving (Show, Eq, Generic)

data Filter = All | Active | Completed
  deriving (Show, Eq, Generic)

newtype LocalStorageTodoItems = LocalStorageTodoItems
  { unLocalStorageTodoItems :: [TodoItem.TodoItemState]
  } deriving newtype (ToJSVal, FromJSVal)

data TodoListAction a where
  InitAction :: ReactiveEnv -> DynRef JSString -> TodoListAction (DynRef TodoListState)
  ToggleAllAction :: TodoListConfig -> Bool -> TodoListAction ()
  InputAction :: TodoListConfig -> JSString -> TodoListAction ()
  CommitAction :: TodoListConfig -> TodoListAction ()
  KeydownAction :: TodoListConfig -> Int -> TodoListAction ()
  DeleteItemAction :: TodoListConfig -> Int -> TodoListAction ()
  ClearCompletedAction :: TodoListConfig -> TodoListAction ()

eval :: TodoListAction a -> Step a
eval = \case
  InitAction renv urlHashRef -> do
    let parseFilter' = fromMaybe All . parseFilter
    todos <- fromMaybe [] . fmap unLocalStorageTodoItems <$> liftIO localStorageGet
    initFilter <- parseFilter' <$> readRef urlHashRef
    todosRef <- execReactiveT renv do
      todosRef <- newRef $ TodoListState "" todos initFilter
      subscribe (updates (fromRef urlHashRef)) $
        modifyRef todosRef . (\v s -> s{filter=v}) . parseFilter'
      return todosRef
    liftIO $ onBeforeUnload do
      TodoListState{items} <- readRef todosRef
      localStorageSet $ LocalStorageTodoItems items
    return todosRef
  ToggleAllAction cfg isChecked ->
    modifyRef cfg.state_ref \s -> s
      { items =
        fmap (\i -> i {TodoItem.completed = isChecked}) s.items
      }
  InputAction cfg newVal -> do
    modifyRef cfg.state_ref \s -> s {title = newVal}
  CommitAction cfg -> do
    title <- JSS.strip . (.title) <$> readRef cfg.state_ref
    case title of
      "" -> return ()
      t -> modifyRef cfg.state_ref \s -> s
        { items = s.items <> [mkNewItem t]
        , title = ""
        }
  KeydownAction cfg key -> case key of
    13 {- Enter -} -> eval (CommitAction cfg)
    _ -> return ()
  DeleteItemAction cfg itemIx ->
    modifyRef cfg.state_ref \s -> s {items = deleteIx itemIx s.items}
  ClearCompletedAction cfg ->
    modifyRef cfg.state_ref \s -> s
      {items = (List.filter (not . TodoItem.completed)) s.items}
  where
    deleteIx :: Int -> [a] -> [a]
    deleteIx _ []     = []
    deleteIx i (a:as)
      | i == 0    = as
      | otherwise = a : deleteIx (i-1) as
    mkNewItem t =
      TodoItem.emptyTodoItemState {TodoItem.title = t}

html :: TodoListConfig -> Html ()
html cfg = do
  el "style" $ text styles
  div_ do
    section_ [class_ "todoapp"] do
      headerWidget
      mainWidget
      footerWidget
    footerInfoWidget
  where
    headerWidget = header_ [class_ "header"] do
      h1_ (text "todos")
      input_ [class_ "new-todo", placeholder_ "What needs to be done?", autofocus_ True] do
        dynValue $ (.title) <$> fromRef cfg.state_ref
        on "input" $ decodeEvent valueDecoder $
          eval . InputAction cfg
        on "keydown" $ decodeEvent keyCodeDecoder $
          eval . KeydownAction cfg
    mainWidget = section_ [class_ "main"] do
      toggleClass "hidden" hiddenDyn
      input_ [id_ "toggle-all", class_ "toggle-all", type_ "checkbox"] do
        on "click" $ decodeEvent checkedDecoder $
          eval . ToggleAllAction cfg
      label_ do
        attr "for" "toggle-all"
        text "Mark all as completed"
      ul_ [class_ "todo-list"] do
        simpleList itemsDyn \idx todoRef ->
          TodoItem.html $ TodoItem.TodoItemConfig
            { TodoItem.state_ref = todoRef
              { dynref_modifier = todoItemModifier cfg idx todoRef.dynref_modifier
              }
            , TodoItem.is_hidden_dyn =
              isTodoItemHidden <$> fromRef cfg.state_ref <*> fromRef todoRef
            , TodoItem.ask_delete_item = eval (DeleteItemAction cfg idx)
            }
    footerWidget = footer_ [class_ "footer"] do
      toggleClass "hidden" hiddenDyn
      span_ [class_ "todo-count"] do
        strong_ $ dynText $ JSS.pack . show <$> itemsLeftDyn
        dynText $ pluralize " item left" " items left" <$> itemsLeftDyn
      ul_ [class_ "filters"] do
        for_ [All, Active, Completed] filterWidget
      button_ [class_ "clear-completed"] do
        on_ "click" $ eval (ClearCompletedAction cfg)
        text "Clear completed"
    footerInfoWidget = footer_ [class_ "info"] do
      p_ "Double-click to edit a todo"
      p_ do
        text "Created by "
        a_ [href_ "https://github.com/lagunoff"] "Vlad Lagunov"
      p_ do
        text "Part of "
        a_ [href_ "http://todomvc.com"] "TodoMVC"
    filterWidget flt = li_ do
      a_ [href_ (printFilter flt)] do
        toggleClass "selected" $ filterSelectedDyn flt
        text $ JSS.pack (show flt)
    hiddenDyn =
      Prelude.null . (.items) <$> fromRef cfg.state_ref
    itemsLeftDyn =
      countItemsLeft <$> fromRef cfg.state_ref
    filterSelectedDyn flt =
      (==flt) . (.filter) <$> fromRef cfg.state_ref
    itemsDyn =
      (.items) <$> fromRef cfg.state_ref
    countItemsLeft TodoListState{items} =
      foldl (\acc TodoItem.TodoItemState{completed} ->
        if not completed then acc + 1 else acc) 0 items
    isTodoItemHidden listState itemState =
      case (listState.filter, itemState.completed) of
        (Active,    True)  -> True
        (Completed, False) -> True
        _                  -> False

-- | Synchronize changes inside TodoItem widget with the outer
-- TodoList widget.
todoItemModifier
  :: TodoListConfig
  -> Int
  -> Modifier TodoItem.TodoItemState
  -> Modifier TodoItem.TodoItemState
todoItemModifier cfg idx elemModifier = Modifier \upd f -> do
  -- Update the local TodoItem element widget
  ((old, new), result) <- unModifier elemModifier upd \old ->
    let (new, result) = f old in (new, ((old, new), result))
  let
    -- When False, the update event won't be propagated into the outer
    -- widget for the sake of optimization
    needsUpdate = upd && (old.completed /= new.completed)
  -- Update the outer widget
  unModifier (dynref_modifier cfg.state_ref) needsUpdate \old ->
    (old {items = overIx idx (const new) old.items}, ())
  return result
  where
    overIx :: Int -> (a -> a) -> [a] -> [a]
    overIx 0 f (x:xs) = f x : xs
    overIx n f (x:xs) = x : overIx (pred n) f xs
    overIx n _ [] = []

pluralize :: JSString -> JSString -> Int -> JSString
pluralize singular plural 0 = singular
pluralize singular plural _ = plural

parseFilter :: JSString -> Maybe Filter
parseFilter =  \case
  "#/"          -> Just All
  "#/active"    -> Just Active
  "#/completed" -> Just Completed
  _             -> Nothing

printFilter :: Filter -> JSString
printFilter =  \case
  All       -> "#/"
  Active    -> "#/active"
  Completed -> "#/completed"

styles :: JSString
styles = "\
  \body {\
  \  margin: 0;\
  \  padding: 0;\
  \}\
  \\
  \button {\
  \  margin: 0;\
  \  padding: 0;\
  \  border: 0;\
  \  background: none;\
  \  font-size: 100%;\
  \  vertical-align: baseline;\
  \  font-family: inherit;\
  \  font-weight: inherit;\
  \  color: inherit;\
  \  -webkit-appearance: none;\
  \  appearance: none;\
  \  -webkit-font-smoothing: antialiased;\
  \  -moz-osx-font-smoothing: grayscale;\
  \}\
  \\
  \body {\
  \  font: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;\
  \  line-height: 1.4em;\
  \  background: #f5f5f5;\
  \  color: #4d4d4d;\
  \  min-width: 230px;\
  \  max-width: 550px;\
  \  margin: 0 auto;\
  \  -webkit-font-smoothing: antialiased;\
  \  -moz-osx-font-smoothing: grayscale;\
  \  font-weight: 300;\
  \}\
  \\
  \:focus {\
  \  outline: 0;\
  \}\
  \\
  \.hidden {\
  \  display: none;\
  \}\
  \\
  \.todoapp {\
  \  background: #fff;\
  \  margin: 130px 0 40px 0;\
  \  position: relative;\
  \  box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2),\
  \              0 25px 50px 0 rgba(0, 0, 0, 0.1);\
  \}\
  \\
  \.todoapp input::-webkit-input-placeholder {\
  \  font-style: italic;\
  \  font-weight: 300;\
  \  color: #e6e6e6;\
  \}\
  \\
  \.todoapp input::-moz-placeholder {\
  \  font-style: italic;\
  \  font-weight: 300;\
  \  color: #e6e6e6;\
  \}\
  \\
  \.todoapp input::input-placeholder {\
  \  font-style: italic;\
  \  font-weight: 300;\
  \  color: #e6e6e6;\
  \}\
  \\
  \.todoapp h1 {\
  \  position: absolute;\
  \  top: -155px;\
  \  width: 100%;\
  \  font-size: 100px;\
  \  font-weight: 100;\
  \  text-align: center;\
  \  color: rgba(175, 47, 47, 0.15);\
  \  -webkit-text-rendering: optimizeLegibility;\
  \  -moz-text-rendering: optimizeLegibility;\
  \  text-rendering: optimizeLegibility;\
  \}\
  \\
  \.new-todo,\
  \.edit {\
  \  position: relative;\
  \  margin: 0;\
  \  width: 100%;\
  \  font-size: 24px;\
  \  font-family: inherit;\
  \  font-weight: inherit;\
  \  line-height: 1.4em;\
  \  border: 0;\
  \  color: inherit;\
  \  padding: 6px;\
  \  border: 1px solid #999;\
  \  box-shadow: inset 0 -1px 5px 0 rgba(0, 0, 0, 0.2);\
  \  box-sizing: border-box;\
  \  -webkit-font-smoothing: antialiased;\
  \  -moz-osx-font-smoothing: grayscale;\
  \}\
  \\
  \.new-todo {\
  \  padding: 16px 16px 16px 60px;\
  \  border: none;\
  \  background: rgba(0, 0, 0, 0.003);\
  \  box-shadow: inset 0 -2px 1px rgba(0,0,0,0.03);\
  \}\
  \\
  \.main {\
  \  position: relative;\
  \  z-index: 2;\
  \  border-top: 1px solid #e6e6e6;\
  \}\
  \\
  \.toggle-all {\
  \  width: 1px;\
  \  height: 1px;\
  \  border: none; /* Mobile Safari */\
  \  opacity: 0;\
  \  position: absolute;\
  \  right: 100%;\
  \  bottom: 100%;\
  \}\
  \\
  \.toggle-all + label {\
  \  width: 60px;\
  \  height: 34px;\
  \  font-size: 0;\
  \  position: absolute;\
  \  top: -52px;\
  \  left: -13px;\
  \  -webkit-transform: rotate(90deg);\
  \  transform: rotate(90deg);\
  \}\
  \\
  \.toggle-all + label:before {\
  \  content: '❯';\
  \  font-size: 22px;\
  \  color: #e6e6e6;\
  \  padding: 10px 27px 10px 27px;\
  \}\
  \\
  \.toggle-all:checked + label:before {\
  \  color: #737373;\
  \}\
  \\
  \.todo-list {\
  \  margin: 0;\
  \  padding: 0;\
  \  list-style: none;\
  \}\
  \\
  \.todo-list li {\
  \  position: relative;\
  \  font-size: 24px;\
  \  border-bottom: 1px solid #ededed;\
  \}\
  \\
  \.todo-list li:last-child {\
  \  border-bottom: none;\
  \}\
  \\
  \.todo-list li.editing {\
  \  border-bottom: none;\
  \  padding: 0;\
  \}\
  \\
  \.todo-list li.editing .edit {\
  \  display: block;\
  \  width: calc(100% - 43px);\
  \  padding: 12px 16px;\
  \  margin: 0 0 0 43px;\
  \}\
  \\
  \.todo-list li.editing .view {\
  \  display: none;\
  \}\
  \\
  \.todo-list li .toggle {\
  \  text-align: center;\
  \  width: 40px;\
  \  /* auto, since non-WebKit browsers doesn't support input styling */\
  \  height: auto;\
  \  position: absolute;\
  \  top: 0;\
  \  bottom: 0;\
  \  margin: auto 0;\
  \  border: none; /* Mobile Safari */\
  \  -webkit-appearance: none;\
  \  appearance: none;\
  \}\
  \\
  \.todo-list li .toggle {\
  \  opacity: 0;\
  \}\
  \\
  \.todo-list li .toggle + label {\
  \  /*\
  \    Firefox requires `#` to be escaped - https://bugzilla.mozilla.org/show_bug.cgi?id=922433\
  \    IE and Edge requires *everything* to be escaped to render, so we do that instead of just the `#` - https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7157459/\
  \  */\
  \  background-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23ededed%22%20stroke-width%3D%223%22/%3E%3C/svg%3E');\
  \  background-repeat: no-repeat;\
  \  background-position: center left;\
  \}\
  \\
  \.todo-list li .toggle:checked + label {\
  \  background-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23bddad5%22%20stroke-width%3D%223%22/%3E%3Cpath%20fill%3D%22%235dc2af%22%20d%3D%22M72%2025L42%2071%2027%2056l-4%204%2020%2020%2034-52z%22/%3E%3C/svg%3E');\
  \}\
  \\
  \.todo-list li label {\
  \  word-break: break-all;\
  \  padding: 15px 15px 15px 60px;\
  \  display: block;\
  \  line-height: 1.2;\
  \  transition: color 0.4s;\
  \}\
  \\
  \.todo-list li.completed label {\
  \  color: #d9d9d9;\
  \  text-decoration: line-through;\
  \}\
  \\
  \.todo-list li .destroy {\
  \  display: none;\
  \  position: absolute;\
  \  top: 0;\
  \  right: 10px;\
  \  bottom: 0;\
  \  width: 40px;\
  \  height: 40px;\
  \  margin: auto 0;\
  \  font-size: 30px;\
  \  color: #cc9a9a;\
  \  margin-bottom: 11px;\
  \  transition: color 0.2s ease-out;\
  \}\
  \\
  \.todo-list li .destroy:hover {\
  \  color: #af5b5e;\
  \}\
  \\
  \.todo-list li .destroy:after {\
  \  content: '×';\
  \}\
  \\
  \.todo-list li:hover .destroy {\
  \  display: block;\
  \}\
  \\
  \.todo-list li .edit {\
  \  display: none;\
  \}\
  \\
  \.todo-list li.editing:last-child {\
  \  margin-bottom: -1px;\
  \}\
  \\
  \.footer {\
  \  color: #777;\
  \  padding: 10px 15px;\
  \  height: 20px;\
  \  text-align: center;\
  \  border-top: 1px solid #e6e6e6;\
  \}\
  \\
  \.footer:before {\
  \  content: '';\
  \  position: absolute;\
  \  right: 0;\
  \  bottom: 0;\
  \  left: 0;\
  \  height: 50px;\
  \  overflow: hidden;\
  \  box-shadow: 0 1px 1px rgba(0, 0, 0, 0.2),\
  \              0 8px 0 -3px #f6f6f6,\
  \              0 9px 1px -3px rgba(0, 0, 0, 0.2),\
  \              0 16px 0 -6px #f6f6f6,\
  \              0 17px 2px -6px rgba(0, 0, 0, 0.2);\
  \}\
  \\
  \.todo-count {\
  \  float: left;\
  \  text-align: left;\
  \}\
  \\
  \.todo-count strong {\
  \  font-weight: 300;\
  \}\
  \\
  \.filters {\
  \  margin: 0;\
  \  padding: 0;\
  \  list-style: none;\
  \  position: absolute;\
  \  right: 0;\
  \  left: 0;\
  \}\
  \\
  \.filters li {\
  \  display: inline;\
  \}\
  \\
  \.filters li a {\
  \  color: inherit;\
  \  margin: 3px;\
  \  padding: 3px 7px;\
  \  text-decoration: none;\
  \  border: 1px solid transparent;\
  \  border-radius: 3px;\
  \}\
  \\
  \.filters li a:hover {\
  \  border-color: rgba(175, 47, 47, 0.1);\
  \}\
  \\
  \.filters li a.selected {\
  \  border-color: rgba(175, 47, 47, 0.2);\
  \}\
  \\
  \.clear-completed,\
  \html .clear-completed:active {\
  \  float: right;\
  \  position: relative;\
  \  line-height: 20px;\
  \  text-decoration: none;\
  \  cursor: pointer;\
  \}\
  \\
  \.clear-completed:hover {\
  \  text-decoration: underline;\
  \}\
  \\
  \.info {\
  \  margin: 65px auto 0;\
  \  color: #bfbfbf;\
  \  font-size: 10px;\
  \  text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);\
  \  text-align: center;\
  \}\
  \\
  \.info p {\
  \  line-height: 1;\
  \}\
  \\
  \.info a {\
  \  color: inherit;\
  \  text-decoration: none;\
  \  font-weight: 400;\
  \}\
  \\
  \.info a:hover {\
  \  text-decoration: underline;\
  \}\
  \\
  \/*\
  \  Hack to remove background from Mobile Safari.\
  \  Can't use it globally since it destroys checkboxes in Firefox\
  \*/\
  \@media screen and (-webkit-min-device-pixel-ratio:0) {\
  \  .toggle-all,\
  \  .todo-list li .toggle {\
  \    background: none;\
  \  }\
  \\
  \  .todo-list li .toggle {\
  \    height: 40px;\
  \  }\
  \}\
  \\
  \@media (max-width: 430px) {\
  \  .footer {\
  \    height: 50px;\
  \  }\
  \\
  \  .filters {\
  \    bottom: 10px;\
  \  }\
  \}"
