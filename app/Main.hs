import Control.Monad
import Control.Exception.Base (bracket)
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
       formatter
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
      fmLookup = lookup formatter formatterMap
    fm <- if isNothing fmLookup
          then do
            putStrLn ("Unknown formatter '" ++ formatter ++ "', defaulting to 'raw'")
            return rawFormatter
          else return (fromJust fmLookup)
    let
      formattedRecipes = map fm recipes
    if output == "-"
    then forM_ formattedRecipes putStrLn
    else do
      bracket
       (openFile output WriteMode)
       (hClose)
       (\h -> forM_
                formattedRecipes
                (hPutStrLn h))


main = do
    options <- execParser optionParser
    run options
