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
tagOpen_ :: Text -> Parser TagToken
tagOpen_ s = satisfy (isTagOpenName s)
tagClose_ :: Text -> Parser TagToken
tagClose_ s = satisfy (isTagCloseName s)


anyTag :: Parser TagToken
anyTag = anyTag_ <* optional anyEmptyTagText
anyTagOpen :: Parser TagToken
anyTagOpen = anyTagOpen_ <* optional anyEmptyTagText
anyTagClose :: Parser TagToken
anyTagClose = anyTagClose_ <* optional anyEmptyTagText
anyTagWarning :: Parser TagToken
anyTagWarning = anyTagWarning_ <* optional anyEmptyTagText
anyTagPosition :: Parser TagToken
anyTagPosition = anyTagPosition_ <* optional anyEmptyTagText
anyTagComment :: Parser TagToken
anyTagComment = anyTagComment_ <* optional anyEmptyTagText
tagOpen :: Text -> Parser TagToken
tagOpen s = tagOpen_ s <* optional anyEmptyTagText
tagClose :: Text -> Parser TagToken
tagClose s = tagClose_ s <* optional anyEmptyTagText

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

section_ str = section str <* optional anyEmptyTagText
