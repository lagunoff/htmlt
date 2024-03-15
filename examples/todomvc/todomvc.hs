import Control.Monad
import Control.Monad.Reader
import HtmlT
import Data.Maybe

import "this" TodoList qualified as TodoList
import "this" Utils

main :: IO ()
main = return ()

foreign export ccall wasm_main :: IO ()
wasm_main = void $ attachToBody do
  reactiveEnv <- lift ask
  urlHashRef <- lift mkUrlHashRef
  let parseFilter = fromMaybe TodoList.All . TodoList.parseFilter
  filter <- parseFilter <$> readRef urlHashRef
  todosListInst <- lift $ TodoList.new filter
  let cfg = TodoList.TodoListConfig todosListInst
  lift $ urlHashRef.dynamic.updates.subscribe \newUrl -> do
    let newFilter = parseFilter newUrl
    TodoList.eval cfg $ TodoList.UpdateFilter newFilter
  liftIO $ onBeforeUnload $ launchReactiveT reactiveEnv $
    TodoList.eval cfg TodoList.PersistCurrentState
  TodoList.html cfg
