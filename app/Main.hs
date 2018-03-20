import Control.Monad
import Chefkoch.CmdLine
import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Html


main = do
    let
      year = 2018
      month = February
    recipes <- fetchRecipes year month
    forM_ recipes print
