{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Chefkoch.CmdLine
import Chefkoch.DataFunctions
import Chefkoch.Format
import Chefkoch.Html.Megaparsec
import Chefkoch.Html.Parser (recipeOfTheDayParser, recipeParser)
import Chefkoch.Http (Grabber (..), download, downloadAndTag)
import Chefkoch.Util
import Control.Exception.Base (bracket)
import Control.Monad
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BC
import Data.Either (partitionEithers)
import Data.List (find, partition)
import Data.Maybe (fromJust)
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
import Text.HTML.TagSoup (ParseOptions (..), Tag (..), isTagPosition, parseOptions, parseTags, parseTagsOptions)
import Text.Megaparsec (runParserT)

run :: Options -> IO ()
run opts@Options {..} = do
  setVerbosity $ if optionVerbose then Loud else Normal
  sayLoud "Starting execution"
  sayLoud $ "Options: " <> show opts
  taggedSources <-
    case optionUrl of
      Nothing -> do
        sayLoud "Choosing recipe of the day..."
        let url = "https://www.chefkoch.de/rezept-des-tages/"
        (html : _) <- download [url]
        eSubUrl <- runParserT (recipeOfTheDayParser url) url (parseTags html)
        case eSubUrl of
          Left err -> do
            putStrLn "Failed to extract today's recipe"
            whenLoud $ rerunParsersDebug [(url, html)] [url] recipeOfTheDayParser
            return []
          Right subUrl -> do
            let url' = "https://www.chefkoch.de" <> subUrl
            sayLoud $ "Using URL: " <> url'
            downloadAndTag [url']
      Just url -> do
        sayLoud $ "Using URL: " <> url
        downloadAndTag [url]
  sayLoud $ "Successfully downloaded " <> show (length taggedSources) <> " recipe pages"
  unless (null taggedSources) $ do
    parseResults <- mapM (\(url, html) -> first (url,) <$> runParserT (recipeParser url) url (parseTags html)) taggedSources
    let (fails, succs) = partitionEithers parseResults
    if null fails
      then putStrLn "All pages were successfully parsed"
      else do
        putStrLn $ show (length fails) <> " pages failed to parse."
        putStrLn $ show (length succs) <> " pages were parsed successfully."
    whenLoud $ unless (null fails) $ rerunParsersDebug taggedSources (fmap fst fails) recipeParser
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

rerunParsersDebug urlSourceLookup urls parser = do
  putStrLn "The following URLs failed:"
  mapM_ (\u -> putStrLn $ "  - " <> u) urls
  putStrLn ""
  putStrLn "Starting to re-run parsers with debug tracing"
  forM_ urls $ \u -> do
    let html = snd . fromJust . find ((== u) . fst) $ urlSourceLookup
        (poss, tags) = partition isTagPosition . parseTagsOptions (parseOptions {optTagPosition = True}) $ html
        posTuples = fmap (\case (TagPosition r c) -> (r, c)) poss
    parseResult <- runParserT (parser u) u tags
    either (showParseErrorWithSource html 10 posTuples) (const . putStrLn $ u <> " did not fail again.") parseResult
  putStrLn "Done"

main = do
  options <- execParser optionParser
  run options
