module HtmlT.IdSupply where

import Data.IORef
import System.IO.Unsafe

newtype Id a = Id {unId :: Int}
  deriving stock (Eq, Ord, Show)

globalRef :: IORef Int
globalRef = unsafePerformIO (newIORef 0)

nextId :: IO (Id a)
nextId = atomicModifyIORef globalRef f where
  f x = (succ x, Id x)
