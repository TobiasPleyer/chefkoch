{-# LANGUAGE OverloadedStrings #-}


module Chefkoch.DataTypes where


import Data.Aeson
import qualified Data.Text as T


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)


instance ToJSON Weekday where
    toJSON = String . T.pack . show


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


instance ToJSON Month where
    toJSON = String . T.pack . show


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


instance ToJSON Recipe where
    toJSON (Recipe
              rDay
              rWeekday
              rMonth
              rYear
              rName
              rUrl
              rIngredients
              rInstructions) = object [
                "Day" .= rDay
              , "Weekday" .= rWeekday
              , "Month" .= rMonth
              , "Year" .= rYear
              , "Name" .= rName
              , "Url" .= rUrl
              , "Ingredients" .= rIngredients
              , "Instructions" .= rInstructions
              ]

