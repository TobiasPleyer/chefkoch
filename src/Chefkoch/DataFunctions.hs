module Chefkoch.DataFunctions where

import Chefkoch.Types

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

monthMaxDays :: Month -> Int
monthMaxDays January = 31
monthMaxDays February = 28
monthMaxDays March = 31
monthMaxDays April = 30
monthMaxDays May = 31
monthMaxDays June = 30
monthMaxDays July = 31
monthMaxDays August = 31
monthMaxDays September = 30
monthMaxDays October = 31
monthMaxDays November = 30
monthMaxDays December = 31

int2Month :: Int -> Maybe Month
int2Month n = case n of
  1 -> Just January
  2 -> Just February
  3 -> Just March
  4 -> Just April
  5 -> Just May
  6 -> Just June
  7 -> Just July
  8 -> Just August
  9 -> Just September
  10 -> Just October
  11 -> Just November
  12 -> Just December
  _ -> Nothing

unsafeInt2Month :: Int -> Month
unsafeInt2Month n = case n of
  1 -> January
  2 -> February
  3 -> March
  4 -> April
  5 -> May
  6 -> June
  7 -> July
  8 -> August
  9 -> September
  10 -> October
  11 -> November
  12 -> December
  _ -> undefined
