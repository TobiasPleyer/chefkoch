import Control.Monad
import Options.Applicative

import Chefkoch.CmdLine
import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Html


main = do
    options <- execParser optionParser
    print options
    --recipes <- fetchRecipes 2018 February
    --forM_ recipes print
