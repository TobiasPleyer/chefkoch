module Chefkoch.Html.Util where


import qualified Data.Text as T
import Text.HTML.TagSoup

import Chefkoch.DataTypes
import Chefkoch.DataFunctions


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


convertFraction :: [Tag T.Text] -> [Tag T.Text]
convertFraction [] = []
convertFraction tags@(t:ts)
  | t ~== "<sup>" = (TagText (innerText (take 8 tags))) : convertFraction (drop 8 tags)
  | otherwise     = t : convertFraction ts


mkPartialRecipe [day, weekday, relative_url, name] =
  let base_url = "https://www.chefkoch.de"
      recipe_url = base_url ++ (T.unpack relative_url)
  in (Recipe
       (Just (read (T.unpack (T.init day))))
       (str2Weekday (T.unpack weekday))
       Nothing
       Nothing
       (T.unpack name)
       recipe_url
       []
       "")
