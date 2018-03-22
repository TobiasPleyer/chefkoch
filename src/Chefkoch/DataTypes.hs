module Chefkoch.DataTypes where


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)


data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Eq, Show)


type Day = Int
type Year = Int


data Recipe = Recipe
    { recipeDay :: Maybe Day
    , recipeWeekday :: Maybe Weekday
    , recipeMonth :: Maybe Month
    , recipeYear :: Maybe Year
    , recipeName :: String
    , recipeUrl :: String
    , recipeIngredients :: [String]
    , recipeInstruction :: String
    } deriving (Eq, Show)
