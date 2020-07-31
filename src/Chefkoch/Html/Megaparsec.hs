{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chefkoch.Html.Megaparsec where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void (..))
import RIO.List (headMaybe)
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Match as TSM
import Text.Megaparsec

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
  take1_ [] = Nothing
  take1_ (t : ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens Proxy = show

  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset offset pstate@PosState {..} = (spos, str, pstate')
    where
      (pre, post) = splitAt (offset - pstateOffset) pstateInput
      newColumn = unPos (sourceColumn pstateSourcePos) + length pre
      spos = pstateSourcePos {sourceColumn = mkPos newColumn}
      str = maybe "<empty line>" show (headMaybe post)
      pstate' =
        pstate
          { pstateInput = post,
            pstateOffset = newColumn,
            pstateSourcePos = spos
          }
  reachOffsetNoLine o pst = undefined

type Parser = Parsec Void TagStream

returnParseResult :: Stream s => Either (ParseErrorBundle s Void) a -> Either String a
returnParseResult res = case res of
  Left err -> Left $ errorBundlePretty err
  Right ok -> Right ok

-- Parser wrappers around the TagSoup matchers

anyTag_ :: Parser TagToken
anyTag_ = satisfy (const True)

anyTagOpen_ :: Parser TagToken
anyTagOpen_ = satisfy isTagOpen

anyTagClose_ :: Parser TagToken
anyTagClose_ = satisfy isTagClose

anyTagWarning_ :: Parser TagToken
anyTagWarning_ = satisfy isTagWarning

anyTagPosition_ :: Parser TagToken
anyTagPosition_ = satisfy isTagPosition

anyTagComment_ :: Parser TagToken
anyTagComment_ = satisfy isTagComment

anyTagText_ :: Parser TagToken
anyTagText_ = satisfy isTagText

tagOpen_ :: Text -> Parser TagToken
tagOpen_ s = satisfy (isTagOpenName s)

tagOpenAttrs_ :: Text -> [Attribute Text] -> Parser TagToken
tagOpenAttrs_ s attrs = satisfy (TagOpen s attrs ~==)

tagOpenAttrNameLit_ :: Text -> Text -> (Text -> Bool) -> Parser TagToken
tagOpenAttrNameLit_ t a p = satisfy (TSM.tagOpenAttrNameLit t a p)

tagClose_ :: Text -> Parser TagToken
tagClose_ s = satisfy (isTagCloseName s)

anyEmptyTagText :: Parser TagToken
anyEmptyTagText = satisfy (\t -> isTagText t && isEmpty (fromTagText t))
  where
    isEmpty = T.null . T.strip

consumeEmptyTags = optional (many (anyEmptyTagText <|> anyTagComment))

anyTag :: Parser TagToken
anyTag = anyTag_ <* consumeEmptyTags

anyTagOpen :: Parser TagToken
anyTagOpen = anyTagOpen_ <* consumeEmptyTags

anyTagClose :: Parser TagToken
anyTagClose = anyTagClose_ <* consumeEmptyTags

anyTagWarning :: Parser TagToken
anyTagWarning = anyTagWarning_ <* consumeEmptyTags

anyTagPosition :: Parser TagToken
anyTagPosition = anyTagPosition_ <* consumeEmptyTags

anyTagComment :: Parser TagToken
anyTagComment = anyTagComment_ <* consumeEmptyTags

anyTagText :: Parser TagToken
anyTagText = anyTagText_ <* consumeEmptyTags

tagOpen :: Text -> Parser TagToken
tagOpen s = tagOpen_ s <* consumeEmptyTags

tagOpenAttrs :: Text -> [Attribute Text] -> Parser TagToken
tagOpenAttrs s attrs = tagOpenAttrs_ s attrs <* consumeEmptyTags

tagOpenAttrNameLit :: Text -> Text -> (Text -> Bool) -> Parser TagToken
tagOpenAttrNameLit t a p = tagOpenAttrNameLit_ t a p <* consumeEmptyTags

tagClose :: Text -> Parser TagToken
tagClose s = tagClose_ s <* consumeEmptyTags

getText :: Parser Text
getText = T.strip . fromTagText <$> anyTagText

getText_ :: Parser Text
getText_ = T.strip . fromTagText <$> anyTagText_

getString :: Parser String
getString = T.unpack <$> getText

findEndTag str = go 0
  where
    go n = anyTag >>= choose n
    choose n t
      | t ~== TagClose str && n == 0 = return ()
      | t ~== TagClose str && n /= 0 = go (n -1)
      | t ~== TagOpen str [] = go (n + 1)
      | otherwise = go n

inside str p = do
  tagOpen str
  r <- p
  findEndTag str
  return r

section str = inside str (pure ())

section_ str = section str <* optional anyEmptyTagText
