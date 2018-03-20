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


str2Weekday :: String -> Weekday
str2Weekday "(Mo)" = Monday
str2Weekday "(Di)" = Tuesday
str2Weekday "(Mi)" = Wednesday
str2Weekday "(Do)" = Thursday
str2Weekday "(Fr)" = Friday
str2Weekday "(Sa)" = Saturday
str2Weekday "(So)" = Sunday
