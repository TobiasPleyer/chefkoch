module Chefkoch.Html.Parser where


import qualified Data.Text as T
import Text.HTML.TagSoup

import Chefkoch.DataTypes
import Chefkoch.Html.Util


parseRecipeTable :: [Tag T.Text] -> [Tag T.Text]
parseRecipeTable = takeWhile (~/= TagClose "table")
                   . tail
                   . dropWhile (~/= TagOpen "table" [("class", "table-day")])


parseMonthlyRecipeListing :: T.Text -> [Recipe]
parseMonthlyRecipeListing =
    map mkPartialRecipe
      . map (map unTag)
      . subGroups 4
      . filter (\t -> notEmptyText t
                   && isNeeded t)
      . normalize
      . parseRecipeTable
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


parseRecipePage :: T.Text -> ([String], String)
parseRecipePage website = (ingredients,instructions)
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
