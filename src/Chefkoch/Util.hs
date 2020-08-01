module Chefkoch.Util where

import Chefkoch.DataFunctions
import Chefkoch.Types
import Data.List
import Data.Maybe
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Time
import System.Console.CmdArgs.Verbosity
import System.Random

sayNormal = whenNormal . putStrLn

sayLoud = whenLoud . putStrLn

getCurrentYearMonthDay :: IO (Year, Month, Day)
getCurrentYearMonthDay = do
  utcTime <- Time.getCurrentTime
  let (y, m, d) = Calendar.toGregorian (Time.utctDay utcTime)
      month = unsafeInt2Month m
      year = fromInteger y
  return (year, month, d)

getRandomYearMonthDay :: IO (Year, Month, Day)
getRandomYearMonthDay = do
  (currYear, currMonth, currDay) <- getCurrentYearMonthDay
  stdGen <- getStdGen
  let (year, stdGen2) = randomR (2003, currYear) stdGen
      (month', stdGen3) =
        if year == currYear
          then randomR (1, month2Int currMonth) stdGen2
          else randomR (1, 12) stdGen2
      month = unsafeInt2Month month'
      (day, _) =
        if month == currMonth
          then randomR (1, currDay) stdGen3
          else randomR (1, monthMaxDays month) stdGen3
  return (year, month, day)

yearMonthFromMaybe :: (Maybe Year, Maybe Month) -> IO (Year, Month)
yearMonthFromMaybe (my, mm) = do
  (currYear, currMonth, currDay) <- getCurrentYearMonthDay
  let year = fromMaybe currYear my
      month = fromMaybe currMonth mm
  return (year, month)

selectRecipesByDay :: Maybe Day -> [(Day, String, String)] -> [(Day, String, String)]
selectRecipesByDay Nothing recipeInfos = recipeInfos
selectRecipesByDay (Just day) recipeInfos =
  case find (\(d, name, url) -> d == day) recipeInfos of
    Nothing -> []
    Just (d, name, url) -> [(d, name, url)]
