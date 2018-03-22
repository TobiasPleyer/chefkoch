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


formatRecipe = show


run :: Options -> IO ()
run (Options
       year
       monthInt
       day
       link
       linksOnly
       random
       output
    ) = do
    let
      month = fmap unsafeInt2Month monthInt
    recipes <- if isJust link
               then do
                 recipe <- wgetDownloadRecipeByLink (fromJust link)
                 return [recipe]
               else wgetDownloadRecipesByDate (year, month, day)
    let
      formattedRecipes = map formatRecipe recipes
    if output == "-"
    then forM_ formattedRecipes putStrLn
    else do
      bracket
       (openFile output WriteMode)
       (hClose)
       (\h -> forM_
                (formattedRecipes)
                (hPutStrLn h))


main = do
    options <- execParser optionParser
    run options
