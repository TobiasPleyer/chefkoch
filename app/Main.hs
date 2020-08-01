{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Chefkoch.CmdLine
import Chefkoch.DataFunctions
import Chefkoch.Format
import Chefkoch.Html.Megaparsec
import Chefkoch.Html.Parser (recipeParser)
import Chefkoch.Http (Grabber (..), downloadAndTag)
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
import Text.HTML.TagSoup (parseTags)
import Text.Megaparsec (runParserT)

run :: Options -> IO ()
run opts@Options {..} = do
  setVerbosity $ if optionVerbose then Loud else Normal
  sayLoud "Starting execution"
  sayLoud $ "Options: " <> show opts
  let month = fmap unsafeInt2Month optionMonth
  sayLoud $ "Month: " <> show month
  taggedSources <-
    if optionRandom
      then do
        sayLoud "Choosing at random..."
        --(year, month, day) <- getRandomYearMonthDay
        --wreqDownloadRecipesByDate optionUrlsOnly (Just year, Just month, Just day)
        return []
      else case optionUrl of
        Just url -> do
          sayLoud $ "Using URL: " <> url
          downloadAndTag [url]
        Nothing ->
          --sayLoud $ "Using (year,month,day): " <> show (optionYear, month, optionDay)
          --wreqDownloadRecipesByDate optionUrlsOnly (optionYear, month, optionDay)
          return []
  sayLoud $ "Successfully downloaded " <> show (length taggedSources) <> " recipes pages"
  unless (null taggedSources) $ do
    parseResults <- mapM (\(url, html) -> runParserT recipeParser url (parseTags html)) taggedSources
    let (fails, succs) = partitionEithers parseResults
    if null fails
      then putStrLn "All pages were successfully parsed"
      else do
        putStrLn $ show (length fails) <> " pages failed to parse."
        putStrLn $ show (length succs) <> " pages were parsed successfully."
    whenLoud $ do
      putStrLn "TODO"
      putStrLn "More info here"
    let maybeFormatter = lookup optionFormat formatterMap
    formatter <- case maybeFormatter of
      Just fm -> return fm
      Nothing -> do
        putStrLn ("Unknown format '" <> optionFormat <> "', defaulting to 'raw'")
        return rawFormatter
    let formattedRecipes = formatter succs
    if optionOutput == "-"
      then BC.putStrLn formattedRecipes
      else BC.writeFile optionOutput formattedRecipes

main = do
  options <- execParser optionParser
  run options
