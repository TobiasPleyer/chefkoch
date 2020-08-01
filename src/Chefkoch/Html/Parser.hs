{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Chefkoch.Html.Parser where

import Chefkoch.DataFunctions (int2Month)
import Chefkoch.Html.Megaparsec
import Chefkoch.Html.Util
import Chefkoch.Types
import Chefkoch.Util (sayLoud)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (filter, takeWhile)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void (..))
import Text.HTML.TagSoup (Tag (..), (~/=), (~==))
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Match as TSM
import Text.Megaparsec
  ( (<|>),
    MonadParsec (..),
    choice,
    getInput,
    many,
    manyTill,
    optional,
    satisfy,
    sepBy1,
    setInput,
    skipManyTill,
  )
import Text.Megaparsec.Debug (dbg)

--parseMonthlyRecipeListing :: Text -> Either String [(Day, Weekday, String, String)]
--parseMonthlyRecipeListing = returnParseResult . parse pMonthlyListing "" . TS.parseTags
--  where
--    pMonthlyListing :: Parser [(Day, Weekday, String, String)]
--    pMonthlyListing = do
--      skipManyTill anyTag pRecipeTableStart
--      rows <- many pRecipeTableRow
--      tagClose "table"
--      return rows
--    pRecipeTableStart :: Parser TagToken
--    pRecipeTableStart = tagOpenAttrNameLit "table" "class" (== "table-day")
--    pRecipeTableRow :: Parser (Day, Weekday, String, String)
--    pRecipeTableRow =
--      inside "tr" $ do
--        (day, weekday) <- inside "td" $ do
--          day <- read . T.unpack . T.init . T.filter (not . isSpace) <$> getText
--          weekday <- inside "span" $ str2Weekday . T.unpack . T.filter (not . isSpace) <$> getText
--          case weekday of
--            Just weekday' -> return (day, weekday')
--            Nothing -> fail "Unable to parse day and weekday"
--        (url, name) <- inside "td" $ do
--          url <- TS.fromAttrib "href" <$> tagOpen "a"
--          name <- getText
--          tagClose "a"
--          return (T.unpack url, T.unpack name)
--        return (day, weekday, name, url)

recipeParser :: String -> Parser IO Recipe
recipeParser url = do
  skipUntil $ tagOpen "h1"
  title <- getString
  liftIO . sayLoud $ "Title: " <> title
  (avg, prep, diff, date, kcal) <- metaParser
  ingredients <- ingredientsParser
  instructions <- instructionsParser
  tags <- tagsParser
  author <- authorParser
  return $ Recipe date title url ingredients instructions (RecipeMeta avg prep diff kcal tags author)

metaParser :: Parser IO (Double, String, String, Date, Maybe Int)
metaParser = do
  avg <- avgParser
  prep <- preptimeParser
  diff <- difficultyParser
  date <- dateParser
  mkcal <- optional kcaloriesParser
  return (avg, prep, diff, date, mkcal)

avgParser :: Parser IO Double
avgParser = do
  skipUntil $ tagOpenAttrNameLit "div" "class" (== "ds-rating-avg")
  skipUntil $ tagOpen "strong"
  txt <- getString
  let avg = read txt
  tagClose "strong"
  liftIO . sayLoud $ "Average: " <> show avg
  return avg

preptimeParser :: Parser IO String
preptimeParser = do
  skipUntil $ tagOpenAttrNameLit "span" "class" (== "recipe-preptime")
  optional (section "i")
  prep <- shrinkWhitespace <$> getText
  tagClose "span"
  liftIO . sayLoud $ "Preparation time: " <> show prep
  return $ T.unpack prep

difficultyParser :: Parser IO String
difficultyParser = do
  tagOpenAttrNameLit "span" "class" (== "recipe-difficulty")
  optional (section "i")
  diff <- shrinkWhitespace <$> getText
  tagClose "span"
  liftIO . sayLoud $ "Difficulty: " <> show diff
  return $ T.unpack diff

dateParser :: Parser IO Date
dateParser = do
  tagOpenAttrNameLit "span" "class" (== "recipe-date")
  optional (section "i")
  date <- shrinkWhitespace <$> getText
  tagClose "span"
  -- TODO: Everything here is pretty unsafe...
  let (d : m : y : _) = T.splitOn "." date
      d' = read . T.unpack $ d
      m' = fromJust . int2Month . read . T.unpack $ m
      y' = read . T.unpack $ y
  liftIO . sayLoud $ "Date: " <> show (d', m', y')
  return (d', m', y')

kcaloriesParser :: Parser IO Int
kcaloriesParser = do
  tagOpenAttrNameLit "span" "class" (== "recipe-kcalories")
  optional (section "i")
  kcalText <- shrinkWhitespace <$> getText
  let kcal = read . takeWhile (not . isSpace) . T.unpack $ kcalText
  tagClose "span"
  liftIO . sayLoud $ "Kilo calories: " <> show kcal
  return kcal

ingredientsParser :: Parser IO [String]
ingredientsParser = do
  skipUntil $ tagOpenAttrNameLit "table" "class" (== "ingredients table-header")
  liftIO . sayLoud $ "Found the start of the ingredients table"
  -- Sometimes a recipe has more than one ingredient table.
  -- To account for that possibility we put the consumed <table> tag back
  -- and then start `many` parses of tables
  input <- getInput
  setInput $ TagOpen "table" [("class", "ingredients table-header")] : input
  ingds <- concat <$> many ingredientsTableParser
  liftIO . sayLoud $ "Finished parsing of the ingredients table"
  return ingds

ingredientsTableParser :: Parser IO [String]
ingredientsTableParser =
  inside "table" $ do
    optional (section "thead")
    inside "tbody" $ many ingredientsTableRowParser

ingredientsTableRowParser :: Parser IO String
ingredientsTableRowParser = do
  row <- inside "tr" $ do
    quantity <- inside "td" $ do
      optional $ tagOpen "span"
      qty <- shrinkWhitespace <$> getAllText
      optional $ tagClose "span"
      return qty
    ingredient <- inside "td" $ inside "span" $ do
      optional $ tagOpen "a"
      ingredient <- shrinkWhitespace <$> getAllText
      optional $ tagClose "a"
      return ingredient
    return $
      if T.null quantity
        then ingredient
        else quantity <> " " <> ingredient
  return $ T.unpack row

instructionsParser :: Parser IO [String]
instructionsParser = do
  skipUntil instructionHeaderParser
  liftIO . sayLoud $ "Found the start of the instructions"
  skipUntil $ tagOpen "div"
  insts <- fmap T.unpack <$> sepBy1 getText (optional (many (section "br")))
  liftIO . sayLoud $ "Finished parsing of the instruction"
  tagClose "div"
  return insts

-- We need the 'try' here because if we find another h2 header and then look into it we have already
-- consumed input. If now we fail because the header text does not match this results in a parse error.
instructionHeaderParser :: Parser IO TagToken
instructionHeaderParser = try $ do
  tagOpen "h2"
  txt <- getText
  guard (txt == "Zubereitung")
  tagClose "h2"

tagsParser :: Parser IO [String]
tagsParser = do
  skipUntil $ tagOpenAttrs "div" [("class", "recipe-tags")]
  liftIO . sayLoud $ "Found the start of the tag list"
  tags <- subParseUntil (tagClose "amp-carousel") $ do
    tags <- getInput
    return . fmap (T.unpack . shrinkWhitespace . TS.fromTagText) . filter (TSM.tagText (const True)) $ tags
  tagClose "div"
  liftIO . sayLoud $ "Finished parsing of the tag list"
  return tags

authorParser :: Parser IO String
authorParser = do
  skipUntil authorHeaderParser
  skipUntil $ tagOpenAttrs "div" [("class", "ds-mb-right")]
  tagOpen "a" >> tagOpen "span"
  T.unpack . shrinkWhitespace <$> getText

-- We need the 'try' here because if we find another h2 header and then look into it we have already
-- consumed input. If now we fail because the header text does not match this results in a parse error.
authorHeaderParser :: Parser IO TagToken
authorHeaderParser = try $ do
  tagOpen "h2"
  txt <- getText
  guard (txt == "Rezept von")
  tagClose "h2"

-- TODO: This is probably not needed anymore
getAllText :: Parser IO Text
getAllText = go ""
  where
    go t = next t <|> return t
    next t = do
      tag <- satisfy textRelated
      if TS.isTagText tag
        then go (t <> T.strip (TS.fromTagText tag))
        else go t
    textRelated (TagComment _) = True
    textRelated (TagText _) = True
    textRelated (TagOpen t _) = t `elem` ["sup", "sub", "b", "i"]
    textRelated (TagClose t) = t `elem` ["sup", "sub", "b", "i"]
    textRelated _ = False

-- Debug wrapped versions of the parsers

recipeParserDbg :: String -> Parser IO Recipe
recipeParserDbg url = dbg "recipeParser" $ recipeParser url

--metaParserDbg :: Parser IO RecipeMeta
--metaParserDbg = dbg "metaParser" metaParser
--
--avgParserDbg :: Parser IO Double
--avgParserDbg = dbg "avgParser" avgParser
--
--preptimeParserDbg :: Parser IO Int
--preptimeParserDbg = dbg "preptimeParser" preptimeParser
--
--difficultyParserDbg :: Parser IO String
--difficultyParserDbg = dbg "difficultyParser" difficultyParser
--
--dateParserDbg :: Parser IO Date
--dateParserDbg = dbg "dateParser" dateParser
--
--kcaloriesParserDbg :: Parser IO Int
--kcaloriesParserDbg = dbg "kcaloriesParser" kcaloriesParser
--
--ingredientsParserDbg :: Parser IO [String]
--ingredientsParserDbg = dbg "ingredientsParser" ingredientsParser
--
--instructionsParserDbg :: Parser IO [String]
--instructionsParserDbg = dbg "instructionsParser" instructionsParser
--
--tagsParserDbg :: Parser IO [String]
--tagsParserDbg = dbg "tagsParser" tagsParser
--
--authorParserDbg :: Parser IO String
--authorParserDbg = dbg "authorParser" authorParser
