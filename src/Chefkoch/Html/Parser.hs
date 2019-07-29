{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Chefkoch.Html.Parser where


import           Control.Monad            (guard)
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


parseMonthlyRecipeListing :: T.Text -> Either String [(Day, Weekday, String, String)]
parseMonthlyRecipeListing = returnParseResult . M.parse pMonthlyListing "" . TS.parseTags
  where
    pMonthlyListing :: Parser [(Day, Weekday, String, String)]
    pMonthlyListing = do
      M.skipManyTill anyTag pRecipeTableStart
      rows <- M.many pRecipeTableRow
      tagClose "table"
      return rows

    pRecipeTableStart :: Parser TagToken
    pRecipeTableStart = tagOpenAttrNameLit "table" "class" (== "table-day")

    pRecipeTableRow :: Parser (Day, Weekday, String, String)
    pRecipeTableRow =
      inside "tr" $ do
        (day, weekday) <- inside "td" $ do
          day <- read . T.unpack . T.init . T.filter (not . isSpace) <$> getText
          weekday <- inside "span" $ str2Weekday . T.unpack . T.filter (not . isSpace) <$> getText
          case weekday of
            Just weekday' -> return (day, weekday')
            Nothing       -> fail "Unable to parse day and weekday"
        (url, name) <- inside "td" $ do
          url <- TS.fromAttrib "href" <$> tagOpen "a"
          name <- getText
          tagClose "a"
          return (T.unpack url, T.unpack name)
        return (day, weekday, name, url)


parseRecipePage :: T.Text -> Either String (String, [String], String)
parseRecipePage = returnParseResult . M.parse pRecipePage "" . TS.parseTags
  where
    pRecipePage = do
      M.skipManyTill anyTag pRecipeTitle
      title <- getString
      M.skipManyTill anyTag pIngredientTableStart
      tagOpen "tbody"
      ingredients <- M.many (T.unpack <$> pIngredientTableRow)
      tagClose "tbody"
      tagClose "table"
      M.skipManyTill anyTag pInstructionsStart
      M.skipManyTill anyTag (tagOpen "div")
      instructions <- T.unpack . T.unlines <$> M.sepBy1 getText (M.optional (M.many (section "br")))
      tagClose "div"
      return (title,ingredients,instructions)

    pRecipeTitle :: Parser TagToken
    pRecipeTitle = tagOpen "h1"

    pIngredientTableStart :: Parser TagToken
    pIngredientTableStart = tagOpenAttrNameLit "table" "class" (== "ingredients table-header")

    pIngredientTableRow :: Parser Text
    pIngredientTableRow = do
      let pSpecials = M.many (M.choice [tagOpen_ "sup", tagClose_ "sup", tagOpen_ "sub", tagClose_ "sub"])
      inside "tr" $ do
        quantity <- inside "td" $ do
          M.optional $ tagOpen "span"
          qty <- getAllText
          M.optional $ tagClose "span"
          return qty
        ingredient <- inside "td" $ inside "span" $ do
          M.optional $ tagOpen "a"
          ingredient <- getAllText
          M.optional $ tagClose "a"
          return ingredient
        return $ if T.null quantity
                 then ingredient
                 else quantity <> " " <> ingredient

    pInstructionsStart :: Parser TagToken
    pInstructionsStart = do
      tagOpen "h2"
      txt <- getText
      guard (txt == "Zubereitung")
      tagClose "h2"

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
            textRelated _                 = False
