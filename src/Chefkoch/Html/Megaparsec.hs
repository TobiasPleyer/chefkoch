{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Chefkoch.Html.Megaparsec where

import           Data.Proxy
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Void         (Void (..))
import           RIO.List          (headMaybe)
import           Text.HTML.TagSoup
import           Text.Megaparsec


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


anyTagText :: Parser TagToken
anyTagText = satisfy isTagText

anyEmptyTagText :: Parser TagToken
anyEmptyTagText = satisfy (\t -> isTagText t && isEmpty (fromTagText t))
  where isEmpty = T.null . T.strip

anyTag :: Parser TagToken
anyTag = satisfy (const True)
anyTagOpen :: Parser TagToken
anyTagOpen = satisfy isTagOpen
anyTagClose :: Parser TagToken
anyTagClose = satisfy isTagClose
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


anyTag_ :: Parser TagToken
anyTag_ = anyTag <* optional anyEmptyTagText
anyTagOpen_ :: Parser TagToken
anyTagOpen_ = anyTagOpen <* optional anyEmptyTagText
anyTagClose_ :: Parser TagToken
anyTagClose_ = anyTagClose <* optional anyEmptyTagText
anyTagWarning_ :: Parser TagToken
anyTagWarning_ = anyTagWarning <* optional anyEmptyTagText
anyTagPosition_ :: Parser TagToken
anyTagPosition_ = anyTagPosition <* optional anyEmptyTagText
anyTagComment_ :: Parser TagToken
anyTagComment_ = anyTagComment <* optional anyEmptyTagText
tagOpen_ :: Text -> Parser TagToken
tagOpen_ s = tagOpen s <* optional anyEmptyTagText
tagClose_ :: Text -> Parser TagToken
tagClose_ s = tagClose s <* optional anyEmptyTagText

--insideTag :: TagToken -> Parser TagToken -> Parser TagToken
--insideTag tok p = do
--    satisfy (~== tok)
--    return $ TagText ""


section str = do
    tagOpen str
    findEndTag 0
    where findEndTag n = do
            t <- anyTag
            if t ~== TagClose str
            then if n == 0
                 then return ()
                 else findEndTag (n-1)
            else if t ~== TagOpen str []
                 then findEndTag (n+1)
                 else findEndTag n
