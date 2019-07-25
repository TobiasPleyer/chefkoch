{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Exception.Base           (bracket)
import           Control.Monad
import qualified Data.ByteString.Char8            as BC
import qualified Data.Text.IO                     as TIO
import           Debug.Trace
import           Options.Applicative
import           System.Console.CmdArgs.Verbosity
import           System.IO                        (IOMode (..), hClose,
                                                   hPutStrLn, openFile)
import           Text.HTML.TagSoup
import qualified Text.Megaparsec                  as M

import           Chefkoch.CmdLine
import           Chefkoch.DataFunctions
import           Chefkoch.Format
import           Chefkoch.Html.Megaparsec
import           Chefkoch.Html.Parser
import           Chefkoch.Http
import           Chefkoch.Util


sayNormal = whenNormal . putStrLn
sayLoud = whenLoud . putStrLn


run :: Options -> IO ()
run opts@Options{..} = do
    setVerbosity Loud
    sayLoud "Starting execution"
    sayLoud $ "Options: " ++ show opts
    let month = fmap unsafeInt2Month optionMonth
    sayLoud $ "Month: " ++ show month
    recipes <- if optionRandom
               then do
                 sayLoud "Choosing at random..."
                 (year,month,day) <- getRandomYearMonthDay
                 wreqDownloadRecipesByDate optionUrlsOnly (Just year, Just month, Just day)
               else
                 case optionUrl of
                 Just url -> do
                   sayLoud $ "Using URL: " ++ url
                   recipe <- wreqDownloadRecipeByUrl url
                   return [recipe]
                 Nothing -> do
                   sayLoud $ "Using (year,month,day): " ++ show (optionYear, month, optionDay)
                   wgetDownloadRecipesByDate optionUrlsOnly (optionYear, month, optionDay)
    sayLoud $ "Found " ++ show (length recipes) ++ " recipes"
    sayLoud $ show recipes
    let maybeFormatter = lookup optionFormat formatterMap
    formatter <- case maybeFormatter of
                 Just fm -> return fm
                 Nothing -> do
                   putStrLn ("Unknown format '" ++ optionFormat ++ "', defaulting to 'raw'")
                   return rawFormatter
    let
      formattedRecipes = formatter recipes
    if optionOutput == "-"
    then BC.putStrLn formattedRecipes
    else BC.writeFile optionOutput formattedRecipes


main = do
    options <- execParser optionParser
    --run options
    text <- TIO.readFile "resources/test.html"
    let tags = parseTags text
        p_res = M.parse test_parser "" tags
    case p_res of
      Left bundle -> putStrLn (M.errorBundlePretty bundle)
      Right (a,b) -> do
        forM_ a TIO.putStrLn
        forM_ b TIO.putStrLn
