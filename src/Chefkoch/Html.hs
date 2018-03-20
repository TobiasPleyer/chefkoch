module Chefkoch.Html where


import Control.Monad
import Data.Char
import Data.List hiding (groupBy)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.StringLike

import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Http
import Chefkoch.Html.Util


extractRecipeTable :: [Tag T.Text] -> [Tag T.Text]
extractRecipeTable = takeWhile (~/= TagClose "table")
                   . tail
                   . dropWhile (~/= TagOpen "table" [("class", "table-day")])


extractRecipeInfo :: T.Text -> [[T.Text]]
extractRecipeInfo = map (map unTag)
                  . subGroups 4
                  . filter (\t -> notEmptyText t
                               && isNeeded t)
                  . normalize
                  . extractRecipeTable
                  . parseTags
    where
      unTag :: Tag T.Text -> T.Text
      unTag (TagText t) = t
      unTag tag@(TagOpen aTag attrs) = fromAttrib (T.pack "href") tag
      unTag _ = T.empty

      isNeeded :: Tag T.Text -> Bool
      isNeeded (TagOpen  t _)
        | t == T.pack "tr"   = False
        | t == T.pack "td"   = False
        | t == T.pack "span" = False
      isNeeded (TagClose t)
        | t == T.pack "tr"   = False
        | t == T.pack "td"   = False
        | t == T.pack "span" = False
        | t == T.pack "a"    = False
      isNeeded _               = True


extractCookingInstructions :: T.Text -> ([String], String)
extractCookingInstructions website = (ingredients,instructions)
  where
    tags = parseTags website
    ingredients = ( map (T.unpack . T.unwords . map fromTagText . filter isTagText)
                  . groupBy "tr"
                  . convertFraction
                  . filter notEmptyText
                  . normalize
                  . takeWhile (~/= "</table>")
                  . tail
                  . dropWhile (~/= "<table class=incredients>")) tags
    instructions = ( T.unpack
                   . innerText
                   . normalize
                   . takeWhile (~/= "</div>")
                   . dropWhile (~/= "<div id=rezept-zubereitung>")) tags


mkRecipe year month [day, weekday, relative_url, name] = do
  let base_url = "https://www.chefkoch.de"
      recipe_url = base_url ++ (T.unpack relative_url)
  webcontent <- wgetURL recipe_url
  let (ingredients,instructions) = extractCookingInstructions webcontent
  return (Recipe
           (read (T.unpack (T.init day)))
           (str2Weekday (T.unpack weekday))
           month
           year
           (T.unpack name)
           recipe_url
           ingredients
           instructions)


fetchRecipes :: Year -> Month -> IO [Recipe]
fetchRecipes year month = do
    webcontent <- fetchRecipeListing wgetURL year month
    let raw_info = extractRecipeInfo webcontent
    mapM (mkRecipe year month) raw_info
