import Control.Monad
import Control.Exception.Base (bracket)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust, isJust, isNothing)
import Options.Applicative
import System.IO (openFile, hPutStrLn, hClose, IOMode(..))

import Chefkoch.CmdLine
import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Util
import Chefkoch.Format


run :: Options -> IO ()
run (Options
       year
       monthInt
       day
       url
       urlsOnly
       random
       output
       format
    ) = do
    let
      month = fmap unsafeInt2Month monthInt
    recipes <- if random
               then do
                 (year,month,day) <- getRandomYearMonthDay
                 wgetDownloadRecipesByDate urlsOnly (Just year, Just month, Just day)
               else
                 if isJust url
                 then do
                   recipe <- wgetDownloadRecipeByUrl (fromJust url)
                   return [recipe]
                 else wgetDownloadRecipesByDate urlsOnly (year, month, day)
    let
      fmLookup = lookup format formatterMap
    formatter <- if isNothing fmLookup
                 then do
                   putStrLn ("Unknown format '" ++ format ++ "', defaulting to 'raw'")
                   return rawFormatter
                 else return (fromJust fmLookup)
    let
      formattedRecipes = formatter recipes
    if output == "-"
    then BC.putStrLn formattedRecipes
    else BC.writeFile output formattedRecipes


main = do
    options <- execParser optionParser
    run options
