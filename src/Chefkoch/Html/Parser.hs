{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Chefkoch.Html.Parser where


import           Data.Char          (isSpace)
import           Data.Either
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import           Data.Semigroup     ((<>))
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Void          (Void (..))
import           RIO.List           (headMaybe)
import           Text.HTML.TagSoup
import           Text.Megaparsec

import           Chefkoch.DataTypes
import           Chefkoch.Html.Util


type TagToken = Tag Text
type TagStream = [TagToken]
instance Stream TagStream where
  type Token TagStream = TagToken
  type Tokens TagStream = TagStream
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ []     = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens Proxy = show
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset offset pstate@PosState{..} = (spos,str,pstate')
    where
      (pre,post) = splitAt (offset - pstateOffset) pstateInput
      newColumn = unPos (sourceColumn pstateSourcePos) + length pre
      spos = pstateSourcePos { sourceColumn = mkPos newColumn }
      str = maybe "<empty line>" show (headMaybe post)
      pstate' = pstate { pstateInput = post
                       , pstateOffset = newColumn
                       , pstateSourcePos = spos
                       }
  reachOffsetNoLine o pst = undefined


type Parser = Parsec Void TagStream

anyTag :: Parser TagToken
anyTag = satisfy (const True)

anyTagOpen :: Parser TagToken
anyTagOpen = satisfy isTagOpen

anyTagClose :: Parser TagToken
anyTagClose = satisfy isTagClose

anyTagText :: Parser TagToken
anyTagText = satisfy isTagText

anyTagWarning :: Parser TagToken
anyTagWarning = satisfy isTagWarning

anyTagPosition :: Parser TagToken
anyTagPosition = satisfy isTagPosition

anyTagComment :: Parser TagToken
anyTagComment = satisfy isTagComment

tagOpen :: Text -> Parser TagToken
tagOpen s = satisfy (isTagOpenName s)

tagClose :: Text -> Parser TagToken
tagClose s = satisfy (isTagCloseName s)

--insideTag :: TagToken -> Parser TagToken -> Parser TagToken
--insideTag tok p = do
--    satisfy (~== tok)
--    return $ TagText ""


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
