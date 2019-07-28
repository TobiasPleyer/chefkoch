module Chefkoch.Html.Util where


import qualified Data.Text              as T
import           Text.HTML.TagSoup

import           Chefkoch.DataFunctions
import           Chefkoch.DataTypes


normalize :: [Tag T.Text] -> [Tag T.Text]
normalize = map (fmap strip)
  where
    strip = T.unwords . T.words


notEmptyText :: Tag T.Text -> Bool
notEmptyText (TagText t)
  | T.null t   = False
notEmptyText _ = True


subGroups :: Int -> [a] -> [[a]]
subGroups n xs
  | length xs < n = []
  | otherwise     = take n xs : subGroups n (drop n xs)


groupBy :: String -> [Tag T.Text] -> [[Tag T.Text]]
groupBy tagString = go
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
  | t ~== "<sup>" = TagText (innerText (take 8 tags)) : convertFraction (drop 8 tags)
  | otherwise     = t : convertFraction ts


mkPartialRecipe (day, weekday, url, name) =
  Recipe
    day
    weekday
    Nothing
    Nothing
    name
    url
    []
    ""
