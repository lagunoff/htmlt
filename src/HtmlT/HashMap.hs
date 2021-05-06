-- module HtmlT.HashMap
--   ( HashMap
--   , module HT
--   ) where

-- import Control.Monad.ST
-- import Data.HashTable.IO as HT
-- import Data.Hashable

-- type HashMap k v = CuckooHashTable k v

module HtmlT.HashMap where

import Data.IORef
import Data.Tuple
import Data.Map as Map

newtype HashMap k v = HashMap {unHashMap :: IORef (Map k v)}
  deriving newtype (Eq)

new :: IO (HashMap k v)
new = HashMap <$> newIORef Map.empty

mutate :: Ord k => HashMap k v -> k -> (Maybe v -> (Maybe v, a)) -> IO a
mutate (HashMap m) k f = atomicModifyIORef' m (swap . Map.alterF (swap . f) k)

lookup :: Ord k => HashMap k v -> k -> IO (Maybe v)
lookup (HashMap m) k = Map.lookup k <$> readIORef m
