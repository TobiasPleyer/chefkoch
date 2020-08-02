module Chefkoch.CmdLine where

import Chefkoch.Types
import Data.Semigroup ((<>))
import Options.Applicative

data Options = Options
  { optionUrl :: Maybe String,
    optionOutput :: String,
    optionFormat :: String,
    optionVerbose :: Bool
  }
  deriving (Show)

chefkochOptions :: Parser Options
chefkochOptions =
  Options
    <$> optional
      ( strOption
          ( long "url"
              <> short 'u'
              <> metavar "URL"
              <> help "The url of the recipe to be downloaded."
          )
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
          <> value "json"
          <> help "Specify the format to be used for output. Supported values: raw, yaml, json (default)."
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Send more info about program execution to stdout."
      )

optionParser =
  info
    (chefkochOptions <**> helper)
    ( fullDesc
        <> progDesc "Download the bare ingredients and cooking informations, without all the clutter."
        <> header "chefkoch - a web crawler for the www.chefkoch.de cooking website"
    )
