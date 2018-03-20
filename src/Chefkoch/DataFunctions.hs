module Chefkoch.DataFunctions where


import Chefkoch.DataTypes


month2Int :: Month -> Int
month2Int January = 1
month2Int February = 2
month2Int March = 3
month2Int April = 4
month2Int May = 5
month2Int June = 6
month2Int July = 7
month2Int August = 8
month2Int September = 9
month2Int October = 10
month2Int November = 11
month2Int December = 12


int2Month :: Int -> Maybe Month
int2Month n  = case n of
  1  -> Just January
  2  -> Just February
  3  -> Just March
  4  -> Just April
  5  -> Just May
  6  -> Just June
  7  -> Just July
  8  -> Just August
  9  -> Just September
  10 -> Just October
  11 -> Just November
  12 -> Just December
  _  -> Nothing


str2Weekday :: String -> Maybe Weekday
str2Weekday "(Mo)" = Just Monday
str2Weekday "(Di)" = Just Tuesday
str2Weekday "(Mi)" = Just Wednesday
str2Weekday "(Do)" = Just Thursday
str2Weekday "(Fr)" = Just Friday
str2Weekday "(Sa)" = Just Saturday
str2Weekday "(So)" = Just Sunday
str2Weekday _      = Nothing


int2Weekday :: Int -> Maybe Weekday
int2Weekday n = case n of
  1 -> Just Monday
  2 -> Just Tuesday
  3 -> Just Wednesday
  4 -> Just Thursday
  5 -> Just Friday
  6 -> Just Saturday
  7 -> Just Sunday
  _ -> Nothing
