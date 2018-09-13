{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Exception.Base (bracket)
import qualified Data.ByteString.Char8 as BC
import Options.Applicative
import System.IO (openFile, hPutStrLn, hClose, IOMode(..))

import Chefkoch.CmdLine
import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Util
import Chefkoch.Format


run :: Options -> IO ()
run Options{..} = do
    let month = fmap unsafeInt2Month optionMonth
    recipes <- if optionRandom
               then do
                 (year,month,day) <- getRandomYearMonthDay
                 wgetDownloadRecipesByDate optionUrlsOnly (Just year, Just month, Just day)
               else
                 case optionUrl of
                 Just url -> do
                   recipe <- wgetDownloadRecipeByUrl url
                   return [recipe]
                 Nothing -> wgetDownloadRecipesByDate optionUrlsOnly (optionYear, month, optionDay)
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
    run options
