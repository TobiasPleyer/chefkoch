import Control.Monad
import Control.Exception.Base (bracket)
import qualified Data.ByteString.Lazy.Char8 as C
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
       link
       linksOnly
       random
       output
       format
    ) = do
    let
      month = fmap unsafeInt2Month monthInt
    recipes <- if random
               then do
                 (year,month,day) <- getRandomYearMonthDay
                 wgetDownloadRecipesByDate linksOnly (Just year, Just month, Just day)
               else
                 if isJust link
                 then do
                   recipe <- wgetDownloadRecipeByLink (fromJust link)
                   return [recipe]
                 else wgetDownloadRecipesByDate linksOnly (year, month, day)
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
    then C.putStrLn formattedRecipes
    else C.writeFile output formattedRecipes


main = do
    options <- execParser optionParser
    run options
