{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenizer
    ( tokenize
    , Token(..)
    , tokenText
    , splitAtParagraphs
    , splitAtSentences
    , isWord
    , isParagraphBreak
    , isSentenceBreak
    , wordCount
    ) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace,take)
import Data.Char (isSpace, isPunctuation)
import Control.Applicative
import Data.List
import Data.Hashable (Hashable(..))

data Token
    = Word !ByteString
    | ParagraphBreak !ByteString
    | SentenceBreak !ByteString
    | Punctuation !ByteString
    | Whitespace !ByteString
    deriving (Eq,Show)

tokenText :: Token -> ByteString
tokenText (Word t) = t
tokenText (ParagraphBreak t) = t
tokenText (SentenceBreak t) = t
tokenText (Punctuation t) = t
tokenText (Whitespace t) = t

instance Hashable Token where
    hash t = hash (tokenText t)

-- | Fully tokenizes a document. This includes locating sentence and paragraph breaks.
-- Because each 'Token' includes its constituent characters, a list of tokens can be concatenated
-- to exactly reconstruct the original document.
tokenize :: ByteString -> [Token]
tokenize txt = createSentenceBreaks $
    case parseOnly (many token) txt of
         Left err -> error err
         Right ts -> ts
    where token = word <|> parBreak <|> whitespace <|> punctuation
          word = Word <$> takeWhile1 isWordChar
          parBreak = ParagraphBreak <$> (string "\n\n" <|> string "\r\n\r\n")
          whitespace = Whitespace <$> takeWhile1 isSpace
          punctuation = Punctuation <$> takeWhile1 (\c -> not (isSpace c) && not (isWordChar c))
          isWordChar = inClass "a-zA-Z0-9_'"

-- For now, simply looks for isolated period characters.
-- Ellipses ("...") will be ignored, but abbreviations ("U.S.") will cause false positives.
-- TODO: use a more sophisticated algorithm.
createSentenceBreaks :: [Token] -> [Token]
createSentenceBreaks = map f
    where f t = if t == Punctuation (BS.pack ".")
                   then SentenceBreak (BS.pack ".")
                   else t


-- | Splits token stream according to 'ParagraphBreak's, discarding them.
splitAtParagraphs :: [Token] -> [[Token]]
splitAtParagraphs toks =
    case findIndex isParagraphBreak toks of
         Just ix -> take ix toks : splitAtParagraphs (drop (ix+1) toks)
         Nothing -> [toks]

-- | Splits token stream according to 'SentenceBreak's, discarding them.
splitAtSentences :: [Token] -> [[Token]]
splitAtSentences toks =
    case findIndex isSentenceBreak toks of
         Just ix -> take ix toks : splitAtSentences (drop (ix+1) toks)
         Nothing -> [toks]

isWord :: Token -> Bool
isWord (Word _) = True
isWord _ = False
isSentenceBreak :: Token -> Bool
isSentenceBreak (SentenceBreak _) = True
isSentenceBreak _ = False
isParagraphBreak :: Token -> Bool
isParagraphBreak (ParagraphBreak _) = True
isParagraphBreak _ = False

wordCount :: [Token] -> Int
wordCount = length . filter isWord

