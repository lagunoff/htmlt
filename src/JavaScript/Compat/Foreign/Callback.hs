{-# LANGUAGE CPP #-}
#if defined(javascript_HOST_ARCH)
module JavaScript.Compat.Foreign.Callback
  ( module GHC.JS.Foreign.Callback
  ) where

import GHC.JS.Foreign.Callback
#else

module JavaScript.Compat.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
      -- * asynchronous callbacks
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
      -- * synchronous callbacks
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
      -- * synchronous callbacks that return a value
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
    ) where

import           JavaScript.Compat.Prim

data OnBlocked = ContinueAsync | ThrowWouldBlock deriving (Eq)

data Callback a

{- |
     When you create a callback, the Haskell runtime stores a reference to
     the exported IO action or function. This means that all data referenced by the
     exported value stays in memory, even if nothing outside the Haskell runtime
     holds a reference to to callback.
     Use 'releaseCallback' to free the reference. Subsequent calls from JavaScript
     to the callback will result in an exception.
 -}
releaseCallback :: Callback a -> IO ()
releaseCallback = undefined

{- | Make a callback (JavaScript function) that runs the supplied IO action in a synchronous
     thread when called.
     Call 'releaseCallback' when done with the callback, freeing memory referenced
     by the IO action.
 -}
syncCallback :: OnBlocked                               -- ^ what to do when the thread blocks
             -> IO ()                                   -- ^ the Haskell action
             -> IO (Callback (IO ()))                   -- ^ the callback
syncCallback = undefined


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes one argument that it passes as a JSVal value to
     the Haskell function.
     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback1 :: OnBlocked                             -- ^ what to do when the thread blocks
              -> (JSVal -> IO ())                      -- ^ the Haskell function
              -> IO (Callback (JSVal -> IO ()))        -- ^ the callback
syncCallback1 = undefined


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes two arguments that it passes as JSVal values to
     the Haskell function.
     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback2 :: OnBlocked                               -- ^ what to do when the thread blocks
              -> (JSVal -> JSVal -> IO ())               -- ^ the Haskell function
              -> IO (Callback (JSVal -> JSVal -> IO ())) -- ^ the callback
syncCallback2 = undefined

{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes three arguments that it passes as JSVal values to
     the Haskell function.
     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback3 :: OnBlocked                               -- ^ what to do when the thread blocks
              -> (JSVal -> JSVal -> JSVal -> IO ())               -- ^ the Haskell function
              -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ())) -- ^ the callback
syncCallback3 = undefined

{- | Make a callback (JavaScript function) that runs the supplied IO action in a synchronous
     thread when called.
     Call 'releaseCallback' when done with the callback, freeing memory referenced
     by the IO action.
 -}
syncCallback' :: IO JSVal
              -> IO (Callback (IO JSVal))
syncCallback' = undefined

syncCallback1' :: (JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> IO JSVal))
syncCallback1' = undefined

syncCallback2' :: (JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback2' = undefined

syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallback3' = undefined

{- | Make a callback (JavaScript function) that runs the supplied IO action in an asynchronous
     thread when called.
     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the IO action.
 -}
asyncCallback :: IO ()              -- ^ the action that the callback runs
              -> IO (Callback (IO ())) -- ^ the callback
asyncCallback = undefined

asyncCallback1 :: (JSVal -> IO ())            -- ^ the function that the callback calls
               -> IO (Callback (JSVal -> IO ())) -- ^ the calback
asyncCallback1 = undefined

asyncCallback2 :: (JSVal -> JSVal -> IO ())            -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSVal -> JSVal -> IO ())) -- ^ the callback
asyncCallback2 = undefined

asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ())               -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ())) -- ^ the callback
asyncCallback3 = undefined

#endif
