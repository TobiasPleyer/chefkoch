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


normalize :: [Tag T.Text] -> [Tag T.Text]
normalize (x:xs) =
  case x of
    TagText t -> TagText (T.unwords (T.words t)) : normalize xs
    _         -> x : normalize xs
normalize [] = []


notEmptyText :: Tag T.Text -> Bool
notEmptyText (TagText t)
  | t == T.empty = False
notEmptyText _   = True


wanted :: Tag T.Text -> Bool
wanted (TagOpen  t _)
  | t == T.pack "tr"   = False
  | t == T.pack "td"   = False
  | t == T.pack "span" = False
wanted (TagClose t)
  | t == T.pack "tr"   = False
  | t == T.pack "td"   = False
  | t == T.pack "span" = False
  | t == T.pack "a"    = False
wanted _               = True


subGroups :: Int -> [a] -> [[a]]
subGroups n xs
  | length xs < n = []
  | otherwise     = (take n xs) : subGroups n (drop n xs)


groupBy :: String -> [Tag T.Text] -> [[Tag T.Text]]
groupBy tagString tags = go tags
  where
    tagText = T.pack tagString
    openTag = TagOpen tagText []
    closeTag = TagClose tagText
    go xs
      | null taken = []
      | length rest < 2 = [taken]
      | otherwise  = taken : go (tail rest)
      where
        (taken,rest) = (span (~/= closeTag) . tail . dropWhile (~/= openTag)) xs


extractRecipeTable :: [Tag T.Text] -> [Tag T.Text]
extractRecipeTable = takeWhile (~/= TagClose "table")
                   . tail
                   . dropWhile (~/= TagOpen "table" [("class", "table-day")])


extractRecipeInfo :: T.Text -> [[T.Text]]
extractRecipeInfo = map (map clearTag)
                  . subGroups 4
                  . filter (\t -> notEmptyText t
                               && wanted t)
                  . normalize
                  . extractRecipeTable
                  . parseTags
    where
      clearTag :: Tag T.Text -> T.Text
      clearTag (TagText t) = t
      clearTag tag@(TagOpen aTag attrs) = fromAttrib (T.pack "href") tag
      clearTag _ = T.empty


handleFractions :: [Tag T.Text] -> [Tag T.Text]
handleFractions [] = []
handleFractions tags@(t:ts)
  | t ~== "<sup>" = (TagText (innerText (take 8 tags))) : handleFractions (drop 8 tags)
  | otherwise     = t : handleFractions ts


extractCookingInstructions :: T.Text -> ([String], String)
extractCookingInstructions website = (ingredients,instructions)
  where
    tags = parseTags website
    ingredients = ( map (T.unpack . T.unwords . map fromTagText . filter isTagText)
                  . groupBy "tr"
                  . handleFractions
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
    webcontent <- fetchRecipeOverview wgetURL year month
    let raw_info = extractRecipeInfo webcontent
    mapM (mkRecipe year month) raw_info
