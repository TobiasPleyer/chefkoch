import Control.Monad
import Data.Maybe (fromJust, isJust, isNothing)
import Options.Applicative

import Chefkoch.CmdLine
import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Util


main = do
    options <- execParser optionParser
    let
      year = optionYear options
      month = fmap unsafeInt2Month (optionMonth options)
      day = optionDay options
      link = optionLink options
      output = optionOutput options
    recipes <- if isJust link
               then do
                 recipe <- wgetDownloadRecipeByLink (fromJust link)
                 return [recipe]
               else wgetDownloadRecipesByDate (year, month, day)
    if output == "-"
    then print recipes
    else do
      writeFile output (show recipes)
