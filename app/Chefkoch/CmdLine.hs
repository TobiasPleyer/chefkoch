module Chefkoch.CmdLine where

import Chefkoch.DataTypes
import Data.Semigroup ((<>))
import Options.Applicative

data Options = Options
  { optionYear :: Maybe Int,
    optionMonth :: Maybe Int,
    optionDay :: Maybe Int,
    optionUrl :: Maybe String,
    optionUrlsOnly :: Bool,
    optionRandom :: Bool,
    optionOutput :: String,
    optionFormat :: String
  }
  deriving (Show)

chefkochOptions :: Parser Options
chefkochOptions =
  Options
    <$> optional
      ( option
          auto
          ( long "year"
              <> short 'y'
              <> metavar "YEAR"
              <> help "The year the recipe was published"
          )
      )
    <*> optional
      ( option
          auto
          ( long "month"
              <> short 'm'
              <> metavar "MONTH"
              <> help "The month the recipe was published"
          )
      )
    <*> optional
      ( option
          auto
          ( long "day"
              <> short 'd'
              <> metavar "DAY"
              <> help "The day the recipe was published"
          )
      )
    <*> optional
      ( strOption
          ( long "url"
              <> short 'u'
              <> metavar "URL"
              <> help "The url of the recipe to be downloaded"
          )
      )
    <*> switch
      ( long "urls-only"
          <> help "Don't look for the ingredients and instructions, just fetch the URLs belonging to the recipes."
      )
    <*> switch
      ( long "random"
          <> short 'r'
          <> help "Whether to choose a recipe at random"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> value "-"
          <> help "The name of the file to write the output to."
      )
    <*> strOption
      ( long "format"
          <> short 'f'
          <> value "yaml"
          <> help "Specify the format to be used for output. Supported values: raw, yaml"
      )

optionParser =
  info
    (chefkochOptions <**> helper)
    ( fullDesc
        <> progDesc "Download the bare ingredients and cooking informations, without all the clutter."
        <> header "chefkoch - a web crawler for the www.chefkoch.de cooking website"
    )
