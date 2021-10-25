import Control.Monad
import Control.Monad.IO.Class
import Gauge
import HtmlT

main = defaultMain
  [ bench "benchDynamics 100 10 10" $ whnfIO (benchDynamics 100 10 10)
  , bench "benchDynamics 10 100 10" $ whnfIO (benchDynamics 10 100 10)
  , bench "benchDynamics 10 10 100" $ whnfIO (benchDynamics 10 10 100)
  ]

benchDynamics :: Int -> Int -> Int -> IO ()
benchDynamics eventsNum subsNum fireNum = reactive do
  -- Create a bunch of 'DynRef's
  refsList <- forM [1..eventsNum] $ const (newRef (0 :: Int))
  outputRef <- newRef Nothing
  -- Sum all of their values into a single 'Dynamic' using
  -- 'Applicative' instance
  let sumDyn = fmap sum . sequenceA . fmap fromRef $ refsList
  -- Attach subsNum amount of subscriptions
  sequence_ $ take subsNum $ repeat $ subscribeAndWrite sumDyn outputRef
  -- And fire modification event for each 'DynRef' fireNum times
  forM_ [1..fireNum] $ const $
    forM_ refsList $ liftIO . sync . flip modifyRef succ
  where
    subscribeAndWrite from to = void $ subscribe (updates from) $
      writeRef to . Just
    reactive act = newReactiveEnv >>= flip runReactiveT act
