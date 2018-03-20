module Chefkoch.Html where


import Data.Maybe (fromJust)
import qualified Data.Text as T
import Text.HTML.TagSoup

import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Html.Util
import Chefkoch.Html.Parser


mkRecipe year month [day, weekday, relative_url, name] = do
  let base_url = "https://www.chefkoch.de"
      recipe_url = base_url ++ (T.unpack relative_url)
  webcontent <- wgetURL recipe_url
  let (ingredients,instructions) = parseCookingInstructions webcontent
  return (Recipe
           (read (T.unpack (T.init day)))
           (fromJust (str2Weekday (T.unpack weekday)))
           month
           year
           (T.unpack name)
           recipe_url
           ingredients
           instructions)


fetchRecipes :: Year -> Month -> IO [Recipe]
fetchRecipes year month = do
    webcontent <- fetchRecipeListing wgetURL year month
    let raw_info = parseRecipeInfo webcontent
    mapM (mkRecipe year month) raw_info
