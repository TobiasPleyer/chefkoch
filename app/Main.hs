{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Exception.Base           (bracket)
import           Control.Monad
import qualified Data.ByteString.Char8            as BC
import           Debug.Trace
import           Options.Applicative
import           System.Console.CmdArgs.Verbosity
import           System.IO                        (IOMode (..), hClose,
                                                   hPutStrLn, openFile)

import           Chefkoch.CmdLine
import           Chefkoch.DataFunctions
import           Chefkoch.DataTypes
import           Chefkoch.Format
import           Chefkoch.Http
import           Chefkoch.Util

import           Chefkoch.Html.Parser
import qualified Data.Text.IO                     as TIO
import           Text.HTML.TagSoup
import qualified Text.Megaparsec                  as M


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


section str = do
    tagOpen str
    findEndTag 0
    where findEndTag n = do
            t <- anyTag
            if t ~== TagClose str
            then if n == 0
                 then return ()
                 else findEndTag (n-1)
            else if t ~== TagOpen str []
                 then findEndTag (n+1)
                 else findEndTag n


parser = do
    tagOpen_ "div"
    tagOpen_ "div"
    tagOpen_ "div"
    tagOpen_ "ul"
    txts <- M.many $ do
             tagOpen_ "li"
             section "span"
             txt <- fromTagText <$> anyTagText
             tagClose_ "li"
             return txt
    M.many anyTag
    return txts


main = do
    options <- execParser optionParser
    --run options
    text <- TIO.readFile "resources/test.html"
    let tags = parseTags text
    M.parseTest parser tags
