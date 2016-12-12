
import Control.Concurrent
import Data.List
import System.Time
import System.Console.ANSI

sample :: CalendarTime
sample = CalendarTime {ctYear = 2016, ctMonth = December, ctDay = 12, ctHour = 19, ctMin = 47, ctSec = 47, ctPicosec = 918513000000, ctWDay = Monday, ctYDay = 346, ctTZName = "EET", ctTZ = 7200, ctIsDST = False}

columns = 44

renderX :: CalendarTime -> (CalendarTime -> Int) -> (Int -> Int) -> Int
renderX calTime f divOperator = divOperator (f calTime)

renderA calTime = renderX calTime ctHour (flip div 5)
renderB calTime = renderX calTime ctHour (flip rem 5)

renderC calTime = renderX calTime ctMin (flip div 5)
renderD calTime = renderX calTime ctMin (flip rem 5)

renderE calTime = renderX calTime ctSec (\x -> if x `rem` 2 == 1 then 0 else 1)

showColumnChar :: Bool -> Char
showColumnChar value = if value then '#' else '_'

showColumn :: Int -> Bool -> String
showColumn width value = " " ++ take (width - 2) (repeat $ showColumnChar value) ++ " "

showRow :: Int -> Int -> String
showRow max cur = take cur (repeat '#') ++ take (max - cur) (repeat '_')

showClockRow :: Int -> Int -> String
showClockRow max cur = concat $ map (showColumn (columns `div` max)) (map (\i -> i <= cur) [1, 2 .. max])

showClockLines :: CalendarTime -> [String]
showClockLines calTime = map (\f -> f calTime) [showClockRow 1 . renderE
                                              , showClockRow 4 . renderA
                                              , showClockRow 4 . renderB
                                              , showClockRow 11 . renderC
                                              , showClockRow 4 . renderD]

showClock :: CalendarTime -> String
showClock calTime = concat (intersperse "\n" (showClockLines calTime))

putClock = do
  clockTime <- getClockTime
  calTime <- toCalendarTime clockTime
  putStrLn (showClock calTime)

runClock = do
  clearScreen
  putClock
  threadDelay (1 * 1000 * 1000)
  runClock
