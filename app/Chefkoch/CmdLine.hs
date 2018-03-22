module Chefkoch.CmdLine where


import Options.Applicative
import Data.Semigroup ((<>))

import Chefkoch.DataTypes


data Options = Options
  { optionYear :: Maybe Int
  , optionMonth :: Maybe Int
  , optionDay :: Maybe Int
  , optionLink :: Maybe String
  , optionLinksOnly :: Bool
  , optionRandom :: Bool
  , optionOutput :: String
  } deriving (Show)


chefkochOptions :: Parser Options
chefkochOptions = Options
         <$> optional (option auto
             ( long "year"
               <> short 'y'
               <> metavar "YEAR"
               <> help "The year the recipe was published"))
         <*> optional (option auto
             ( long "month"
               <> short 'm'
               <> metavar "MONTH"
               <> help "The month the recipe was published"))
         <*> optional (option auto
             ( long "day"
               <> short 'd'
               <> metavar "DAY"
               <> help "The day the recipe was published"))
         <*> optional (strOption
             ( long "link"
               <> short 'l'
               <> metavar "LINK"
               <> help "The link (url) of the recipe to be downloaded"))
         <*> switch
             ( long "links-only"
             <> help "Don't look for the ingredients and instructions, just for the links.")
         <*> switch
             ( long "random"
             <> short 'r'
             <> help "Whether to choose a recipe at random")
         <*> strOption
             ( long "output"
               <> short 'o'
               <> metavar "FILE"
               <> value "recipe"
               <> help "The link (url) of the recipe to be downloaded")


optionParser = info (chefkochOptions <**> helper)
  (  fullDesc
  <> progDesc "Download the bare ingredients and cooking informations, without all the clutter."
  <> header "chefkoch - a web crawler for the www.chefkoch.de cooking website" )
