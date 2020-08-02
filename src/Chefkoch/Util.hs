module Chefkoch.Util where

import Chefkoch.DataFunctions
import Chefkoch.Html.Megaparsec
import Chefkoch.Types
import Control.Monad (forM_)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Time
import System.Console.CmdArgs.Verbosity
import System.Random
import Text.Megaparsec (PosState (..))
import Text.Megaparsec.Error (ParseError (..), ParseErrorBundle (..), ShowErrorComponent, errorOffset, parseErrorPretty)
import Text.Megaparsec.Stream (Stream)

sayNormal = whenNormal . putStrLn

sayLoud = whenLoud . putStrLn

tshow :: Show a => a -> Text
tshow = T.pack . show

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

showParseErrorWithSource :: (Show s, Stream s, ShowErrorComponent e) => Text -> Int -> [(Int, Int)] -> ParseErrorBundle s e -> IO ()
showParseErrorWithSource source ctx poss (ParseErrorBundle errs posState) = do
  let sourceLines = T.lines source
  forM_ (NE.toList errs) $ \e -> do
    let offset = errorOffset e
        offset' = min offset (length poss - 1)
        (row, col) = poss !! offset'
        -- TODO: This is not sophisticated enough
        relevantLines = take (2 * ctx) . drop (row - ctx) $ sourceLines
    putStrLn $ parseErrorPretty e
    putStrLn ""
    sayLoud $ "Problem occurred in row " <> show row <> " column " <> show col
    putStrLn . T.unpack . T.unlines $ relevantLines
