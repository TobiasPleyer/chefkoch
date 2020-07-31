{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Chefkoch.CmdLine
import Chefkoch.DataFunctions
import Chefkoch.Format
import Chefkoch.Html.Megaparsec
import Chefkoch.Html.Parser
import Chefkoch.Http
import Chefkoch.Util
import Control.Exception.Base (bracket)
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Either (partitionEithers)
import Data.List (partition)
import qualified Data.Text.IO as TIO
import Debug.Trace
import Options.Applicative
import System.Console.CmdArgs.Verbosity
import System.IO
  ( IOMode (..),
    hClose,
    hPutStrLn,
    openFile,
  )
import Text.HTML.TagSoup
import qualified Text.Megaparsec as M

run :: Options -> IO ()
run opts@Options {..} = do
  setVerbosity Quiet
  sayLoud "Starting execution"
  sayLoud $ "Options: " ++ show opts
  let month = fmap unsafeInt2Month optionMonth
  sayLoud $ "Month: " ++ show month
  recipes <-
    if optionRandom
      then do
        sayLoud "Choosing at random..."
        (year, month, day) <- getRandomYearMonthDay
        wreqDownloadRecipesByDate optionUrlsOnly (Just year, Just month, Just day)
      else case optionUrl of
        Just url -> do
          sayLoud $ "Using URL: " ++ url
          wreqDownloadRecipesByUrl [url]
        Nothing -> do
          sayLoud $ "Using (year,month,day): " ++ show (optionYear, month, optionDay)
          wreqDownloadRecipesByDate optionUrlsOnly (optionYear, month, optionDay)
  sayLoud $ "Found " ++ show (length recipes) ++ " recipes"
  unless (null recipes) $ do
    let maybeFormatter = lookup optionFormat formatterMap
    formatter <- case maybeFormatter of
      Just fm -> return fm
      Nothing -> do
        putStrLn ("Unknown format '" ++ optionFormat ++ "', defaulting to 'raw'")
        return rawFormatter
    let formattedRecipes = formatter recipes
    if optionOutput == "-"
      then BC.putStrLn formattedRecipes
      else BC.writeFile optionOutput formattedRecipes

main = do
  options <- execParser optionParser
  run options
