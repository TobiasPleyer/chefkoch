import Control.Monad
import Data.Maybe (fromJust)
import Options.Applicative

import Chefkoch.CmdLine
import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Html


main = do
    options <- execParser optionParser
    recipes <- fetchRecipes ((fromJust . optionYear) options) ((fromJust . int2Month . fromJust . optionMonth) options)
    forM_ recipes print
