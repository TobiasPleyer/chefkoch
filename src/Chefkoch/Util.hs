module Chefkoch.Util where


import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Calendar

import Chefkoch.DataTypes
import Chefkoch.DataFunctions


getCurrentYearMonthDay :: IO (Year,Month,Day)
getCurrentYearMonthDay = do
  utcTime <- Time.getCurrentTime
  let (y,m,d) = Calendar.toGregorian (Time.utctDay utcTime)
      month = unsafeInt2Month m
      year = fromInteger y
  return (year, month, d)


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
