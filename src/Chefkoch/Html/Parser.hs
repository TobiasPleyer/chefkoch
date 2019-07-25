{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Chefkoch.Html.Parser where


import           Data.Char                (isSpace)
import           Data.Either
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Proxy
import           Data.Semigroup           ((<>))
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Void                (Void (..))
import           RIO.List                 (headMaybe)
import           Text.HTML.TagSoup
import qualified Text.Megaparsec          as M

import           Chefkoch.DataTypes
import           Chefkoch.Html.Megaparsec
import           Chefkoch.Html.Util


parser = do
    tagOpen "div"
    tagOpen "div"
    tagOpen "div"
    tagOpen "ul"
    txts <- M.many $ do
             tagOpen "li"
             section_ "span"
             txt <- fromTagText <$> anyTagText
             tagClose "li"
             return txt
    M.many anyTag
    return txts


extractRecipeTable = undefined
-- extractRecipeTable :: [Tag T.Text] -> Either Error [Tag T.Text]
--extractRecipeTable ts = do
--                          ts' <- pure $ dropWhile (~/= TagOpen "table" [("class", "table-day")]) ts
--                          ts'' <- case L.tailMaybe ts' of
--                                    Nothing -> Left "Error extracting recipe table, couldn't find the start of the table"
--                                    Just ts''' -> Right ts'''
--                          return $ takeWhile (~/= TagClose "table") ts''


parseMonthlyRecipeListing = undefined
--parseMonthlyRecipeListing :: T.Text -> [Recipe]
--parseMonthlyRecipeListing =
--    map (mkPartialRecipe . map unTag)
--      . subGroups 4
--      . filter (\t -> notEmptyText t
--                   && isNeeded t)
--      . normalize
--      . extractRecipeTable
--      . parseTags
--    where
--      unTag :: Tag T.Text -> T.Text
--      unTag (TagText t)              = t
--      unTag tag@(TagOpen aTag attrs) = fromAttrib (T.pack "href") tag
--      unTag _                        = T.empty
--
--      isNeeded :: Tag T.Text -> Bool
--      isNeeded (TagOpen  t _)
--        | t == T.pack "tr"   = False
--        | t == T.pack "td"   = False
--        | t == T.pack "span" = False
--      isNeeded (TagClose t)
--        | t == T.pack "tr"   = False
--        | t == T.pack "td"   = False
--        | t == T.pack "span" = False
--        | t == T.pack "a"    = False
--      isNeeded _             = True
--
--
parseRecipePage = undefined
--parseRecipePage :: T.Text -> (String, [String], String)
--parseRecipePage website = (title,ingredients,instructions)
--  where
--    tags = parseTags website
--    title = ( T.unpack
--            . fromAttrib (T.pack "content")
--            . head
--            . dropWhile (~/= "<meta property=og:title>")) tags
--    ingredients = ( map (T.unpack . T.unwords . map fromTagText . filter isTagText)
--                  . groupBy "tr"
--                  . convertFraction
--                  . filter notEmptyText
--                  . normalize
--                  . takeWhile (~/= "</table>")
--                  . tail
--                  . dropWhile (~/= "<table class=incredients>")) tags
--    instructions = ( T.unpack
--                   . innerText
--                   . normalize
--                   . takeWhile (~/= "</div>")
--                   . dropWhile (~/= "<div id=rezept-zubereitung>")) tags
