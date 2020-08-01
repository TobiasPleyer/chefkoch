{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Chefkoch.Types where

import Data.Aeson
import GHC.Generics

data Month
  = January
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
  deriving (Eq, Show, Generic)

instance ToJSON Month

type Day = Int

type Year = Int

data RecipeMeta = RecipeMeta
  { metaRating :: Double,
    metaPrepTime :: Int,
    metaDifficulty :: String,
    metaKCalories :: Int,
    metaTags :: [String],
    metaAuthor :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON RecipeMeta

type Date = (Day, Month, Year)

data Recipe = Recipe
  { recipeDate :: Date,
    recipeName :: String,
    recipeUrl :: String,
    recipeIngredients :: [String],
    recipeInstruction :: [String],
    recipeMeta :: RecipeMeta
  }
  deriving (Eq, Show, Generic)

instance ToJSON Recipe
