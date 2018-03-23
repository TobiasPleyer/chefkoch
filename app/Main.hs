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


run :: Options -> IO ()
run (Options
       year
       monthInt
       day
       link
       linksOnly
       random
       output
       raw
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
      formatter = if raw then show else formatRecipe
      formattedRecipes = map formatter recipes
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
