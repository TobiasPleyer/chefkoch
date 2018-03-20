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
    { day :: Day
    , weekday :: Weekday
    , month :: Month
    , year :: Year
    , name :: String
    , url :: String
    , ingredients :: [String]
    , instruction :: String
    } deriving (Eq, Show)
