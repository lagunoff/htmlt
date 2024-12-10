{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds #-}
module StopWatch where

import Clickable
import Data.Time
import Control.Monad.IO.Class
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Int
import Control.Monad
import Data.String

data StopWatchInstance = StopWatchInstance {
  time_var :: DynVar (Maybe UTCTime, Maybe IntervalId)
}

data StopWatchState = StopWatchState {
  t0 :: Maybe UTCTime,
  iid :: Maybe IntervalId
}

newtype IntervalId = IntervalId {unIntervalId :: Int32}
  deriving (ToJSVal, FromJSVal)

run :: JSM ()
run = do
  timeVar <- newVar $ StopWatchState Nothing Nothing
  clockEv <- newEvent
  clockEv `subscribeEvent` \_ -> modifyVar_ timeVar Prelude.id
  execHTMLBody do
    el "style" $ text styles
    liftJSM $ addEventListener spaceKeyEvent $ const $ toggleState clockEv timeVar
    div_ [class_ "StopWatch-container"] do
      h1_ [class_ "StopWatch-time"] do
        dynText $ MapIOVal (fromVar timeVar) prettyTime'
      button_ [class_ "StopWatch-button"] do
        dynText $ fmap buttonTitle $ fromVar timeVar
        on @"click" $ toggleState clockEv timeVar
    t0 <- liftIO getCurrentTime
    month t0
  where
    -- updateTime :: UTCTime -> Maybe (UTCTime, IntervalId) -> Maybe (UTCTime, IntervalId)
    -- updateTime _ Nothing = Nothing
    -- updateTime t (Just (_, i)) = Just (t, i)

    toggleState :: Event () -> DynVar StopWatchState -> JSM ()
    toggleState clockEv self = do
      liftIO $ print $ "toggleState"
      s <- readVar self
      case s of
        StopWatchState Nothing Nothing -> do
          iid <- setInterval clockEv
          t0 <- liftIO getCurrentTime
          writeVar self $ StopWatchState (Just t0) (Just iid)
        StopWatchState (Just t) (Just iid) -> do
          clearInterval iid
          writeVar self $ StopWatchState (Just t) Nothing
        StopWatchState _ _ -> do
          writeVar self $ StopWatchState Nothing Nothing

    prettyTime' :: StopWatchState -> IO Text
    prettyTime' (StopWatchState Nothing _) = pure "00:00:000"
    prettyTime' (StopWatchState (Just t0) _) = do
      t1 <- getCurrentTime
      pure $ stopwatch t0 t1

    buttonTitle :: StopWatchState -> Text
    buttonTitle (StopWatchState Nothing Nothing) = "Start"
    buttonTitle (StopWatchState (Just _) (Just _)) = "Stop"
    buttonTitle (StopWatchState _ _) = "Reset"

spaceKeyEvent :: Event () -> JSExp
spaceKeyEvent (Event eventId) =
  Eval script `Apply` [Lam (TriggerEvent eventId (Arg 0))]
  where
    script =
      "(function(trigger){\n\
      \  function listener(event){\n\
      \    if (event.target.classList.contains('StopWatch-button')) return;\n\
      \    if (event.keyCode != 32) return;\n\
      \    trigger();\n\
      \  }\n\
      \  window.addEventListener('keydown', listener);\n\
      \  return () => window.removeEventListener('keydown', listener);\n\
      \})"

setInterval :: Event () -> JSM IntervalId
setInterval (Event eid) = do
  v <- jsEval $ Eval "trigger => setInterval(trigger, 1)" `Apply` [Lam (TriggerEvent eid Null)]
  maybe (error "setInterval: invalid result") pure $ fromJSVal v

clearInterval :: IntervalId -> JSM ()
clearInterval intervalId =
  jsCmd $ Apply (Id "clearInterval") [toJSVal intervalId]

-- | Calculate the difference between two moments in time and format it as mm:ss:mmmm
stopwatch :: UTCTime -> UTCTime -> Text
stopwatch begin now =
  let diffSeconds = realToFrac $ diffUTCTime now begin :: Double
      totalMilliseconds = round (diffSeconds * 1000) :: Int
      minutes = totalMilliseconds `div` (60 * 1000)
      seconds = (totalMilliseconds `div` 1000) `mod` 60
      milliseconds = totalMilliseconds `mod` 1000
  in pad2 minutes <> ":" <> pad2 seconds <> ":" <> pad4 milliseconds

-- | Helper function to pad numbers to 2 digits
pad2 :: Int -> Text
pad2 n = if n < 10 then "0" <> show' n else show' n

-- | Helper function to pad numbers to 4 digits
pad4 :: Int -> Text
pad4 n
  | n < 10    = "00" <> show' n
  | n < 100   = "0"  <> show' n
  | n < 1000  =         show' n
  | otherwise = show' n

-- | Format a given UTCTime as HH:mm:ss:mmmm
prettyTime :: UTCTime -> Text
prettyTime time =
  let dayTime = utctDayTime time
      timeOfDay = timeToTimeOfDay dayTime
      hours = todHour timeOfDay
      minutes = todMin timeOfDay
      seconds = floor (todSec timeOfDay) :: Int
      milliseconds = round ((todSec timeOfDay - fromIntegral seconds) * 1000) :: Int
  in pad2 hours <> ":" <> pad2 minutes <> ":" <> pad2 seconds <> ":" <> pad4 milliseconds

show' :: Show a => a -> Text
show' = Text.pack . show

styles :: Text
styles = "\
  \body {\
  \  font-family: arial;\
  \}\
  \.StopWatch-container {\
  \  margin: 0 auto;\
  \  text-align: center;\
  \}\
  \.StopWatch-time {\
  \  font-family: system-ui;\
  \  font-size: 196px;\
  \  font-weight: 400;\
  \}\
  \\
  \.StopWatch-button {\
  \  font-size: 48px;\
  \  border: none;\
  \  background: blue;\
  \  color: white;\
  \  border-radius: 32px;\
  \  padding: 4px 22px;\
  \  cursor: pointer;\
  \}\
  \"

-- Here is how to create a table using my library. Complete the
-- function that builds calendar layout for given month
makeTable :: HTML ()
makeTable = do
  table_ [class_ "table"] do
    tbody_ do
      tr_ do td_ "One"; td_ "Two"; td_ "Three"

month :: UTCTime -> HTML ()
month t = do
  let day          = utctDay t
      (y, m, _)    = toGregorian day
      firstDay      = fromGregorian y m 1
      daysInMonth   = gregorianMonthLength y m
      firstWeekDay  = dayOfWeek firstDay
      startOffset   = (fromEnum firstWeekDay - 1) `mod` 7
      days          = [1..daysInMonth]
      paddedDays    = replicate startOffset "" ++ map show days

  table_ [class_ "table"] $ do
    thead_ $ tr_ $ forM_ ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"] (th_ . fromString)
    tbody_ $ forM_ (chunksOf 7 paddedDays) $ \week ->
      tr_ $ forM_ week (td_ . fromString)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n <= 0    = error "chunksOf: chunk size must be positive"
  | otherwise = take n xs : chunksOf n (drop n xs)
