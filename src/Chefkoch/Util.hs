module Chefkoch.Util where


import           Data.List
import           Data.Maybe
import qualified Data.Time.Calendar               as Calendar
import qualified Data.Time.Clock                  as Time
import           System.Console.CmdArgs.Verbosity
import           System.Random

import           Chefkoch.DataFunctions
import           Chefkoch.DataTypes


sayNormal = whenNormal . putStrLn
sayLoud = whenLoud . putStrLn


getCurrentYearMonthDay :: IO (Year,Month,Day)
getCurrentYearMonthDay = do
  utcTime <- Time.getCurrentTime
  let (y,m,d) = Calendar.toGregorian (Time.utctDay utcTime)
      month = unsafeInt2Month m
      year = fromInteger y
  return (year, month, d)


getRandomYearMonthDay :: IO (Year,Month,Day)
getRandomYearMonthDay = do
  (currYear,currMonth,currDay) <- getCurrentYearMonthDay
  stdGen <- getStdGen
  let
    (year,stdGen2) = randomR (2003,currYear) stdGen
    (month',stdGen3) = if year == currYear
                       then randomR (1,month2Int currMonth) stdGen2
                       else randomR (1,12) stdGen2
    month = unsafeInt2Month month'
    (day,_) = if month == currMonth
              then randomR (1,currDay) stdGen3
              else randomR (1,monthMaxDays month) stdGen3
  return (year, month, day)


yearMonthFromMaybe :: (Maybe Year, Maybe Month) -> IO (Year, Month)
yearMonthFromMaybe (my,mm) = do
  (currYear,currMonth,currDay) <- getCurrentYearMonthDay
  let
    year = fromMaybe currYear my
    month = fromMaybe currMonth mm
  return (year,month)


selectRecipesByDay :: Maybe Day -> [Recipe] -> [Recipe]
selectRecipesByDay Nothing recipes = recipes
selectRecipesByDay maybeDay recipes =
  let maybeRecipe = find (\r -> recipeDay r == maybeDay) recipes
  in
    if isNothing maybeRecipe
    then []
    else [fromJust maybeRecipe]



emptyRecipe = Recipe Nothing Nothing Nothing Nothing "" "" [] ""


modifyRecipeYear :: Maybe Year -> Recipe -> Recipe
modifyRecipeYear y r = r{recipeYear=y}

modifyRecipeMonth :: Maybe Month -> Recipe -> Recipe
modifyRecipeMonth m r = r{recipeMonth=m}

modifyRecipeDay :: Maybe Day -> Recipe -> Recipe
modifyRecipeDay d r = r{recipeDay=d}

modifyRecipeWeekday :: Maybe Weekday -> Recipe -> Recipe
modifyRecipeWeekday w r = r{recipeWeekday=w}

modifyRecipeUrl :: String -> Recipe -> Recipe
modifyRecipeUrl u r = r{recipeUrl=u}

modifyRecipeName :: String -> Recipe -> Recipe
modifyRecipeName n r = r{recipeUrl=n}

modifyRecipeIngredients :: [String] -> Recipe -> Recipe
modifyRecipeIngredients ingr r = r{recipeIngredients=ingr}

modifyRecipeInstruction :: String -> Recipe -> Recipe
modifyRecipeInstruction inst r = r{recipeInstruction=inst}
