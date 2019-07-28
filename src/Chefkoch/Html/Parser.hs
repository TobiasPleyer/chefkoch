{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Chefkoch.Html.Parser where


import           Data.Char                (isSpace)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Void                (Void (..))
import           Text.HTML.TagSoup        ((~/=), (~==))
import qualified Text.HTML.TagSoup        as TS
import qualified Text.Megaparsec          as M
import           Text.Megaparsec.Debug    (dbg)

import           Chefkoch.DataFunctions
import           Chefkoch.DataTypes
import           Chefkoch.Html.Megaparsec
import           Chefkoch.Html.Util


parseMonthlyRecipeListing :: T.Text -> Either String [Recipe]
parseMonthlyRecipeListing = returnParseResult . M.parse pMonthlyListing "" . TS.parseTags
  where
    pMonthlyListing :: Parser [Recipe]
    pMonthlyListing = do
      M.skipManyTill anyTag pRecipeTableStart
      rows <- M.many pRecipeTableRow
      tagClose "table"
      return $ map mkPartialRecipe rows

    pRecipeTableStart :: Parser TagToken
    pRecipeTableStart = tagOpenAttrNameLit "table" "class" (== "table-day")

    pRecipeTableRow :: Parser (Maybe Day, Maybe Weekday, String, String)
    pRecipeTableRow =
      inside "tr" $ do
        (day, weekday) <- inside "td" $ do
          day <- read . T.unpack . T.init . T.filter (not . isSpace) <$> getText
          weekday <- inside "span" $ str2Weekday . T.unpack . T.filter (not . isSpace) <$> getText
          case weekday of
            Just weekday' -> return (Just day, Just weekday')
            Nothing       -> fail "Unable to parse day and weekday"
        (url, name) <- inside "td" $ do
          url <- TS.fromAttrib "href" <$> tagOpen "a"
          name <- getText
          tagClose "a"
          return (T.unpack url, T.unpack name)
        return (day, weekday, url, name)


parseRecipePage :: T.Text -> Either String (String, [String], String)
parseRecipePage = returnParseResult . M.parse pRecipePage "" . TS.parseTags
  where
    pRecipePage = do
      dbg "Skipping to title start" $ M.skipManyTill anyTag pRecipeTitle
      title <- dbg "title" getString
      dbg "Skipping to ingredient table start" $ M.skipManyTill anyTag pIngredientTableStart
      rows <- dbg "Ingredients" $ M.many (T.unpack <$> pIngredientTableRow)
      tagClose "table"
      dbg "Skipping to instructions start" $ M.skipManyTill anyTag pInstructionsStart
      instructions <- dbg "instructions" $ T.unpack . T.unlines <$> M.sepBy1 getText (M.optional (M.many (section "br")))
      tagClose "div"
      return (title,rows,instructions)

    pRecipeTitle :: Parser TagToken
    pRecipeTitle = tagOpenAttrNameLit "h1" "class" (== "page-title")

    pIngredientTableStart :: Parser TagToken
    pIngredientTableStart = tagOpenAttrNameLit "table" "class" (== "incredients")

    pIngredientTableRow :: Parser Text
    pIngredientTableRow = do
      let pSpecials = M.many (M.choice [tagOpen_ "sup", tagClose_ "sup", tagOpen_ "sub", tagClose_ "sub"])
      inside "tr" $ do
        quantity <- inside "td" getAllText
        ingredient <- dbg "Ingredient" $ inside "td" $ do
          dbg "Maybe <a>" $ M.optional $ tagOpen "a"
          ingredient <- dbg "Inner Ingredient" getAllText
          dbg "Maybe </a>" $ M.optional $ tagClose "a"
          return ingredient
        return $ if T.null quantity
                 then ingredient
                 else quantity <> " " <> ingredient

    pInstructionsStart :: Parser TagToken
    pInstructionsStart = tagOpenAttrNameLit "div" "id" (== "rezept-zubereitung")

    getAllText :: Parser Text
    getAllText = go ""
      where go t = next t M.<|> return t

            next t = do
              tag <- M.satisfy textRelated
              if TS.isTagText tag
              then go (t <> T.strip (TS.fromTagText tag))
              else go t

            textRelated (TS.TagComment _) = True
            textRelated (TS.TagText _)    = True
            textRelated (TS.TagOpen t _)  = t `elem` ["sup", "sub", "b", "i"]
            textRelated (TS.TagClose t)   = t `elem` ["sup", "sub", "b", "i"]
