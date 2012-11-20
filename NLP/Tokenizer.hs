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
import Data.Char (isSpace)
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
splitAtParagraphs = splitAtToken isParagraphBreak

-- | Splits token stream according to 'SentenceBreak's, discarding them.
-- 'ParagraphBreak's are also taken into account as being implicit 'SentenceBreak's.
splitAtSentences :: [Token] -> [[Token]]
splitAtSentences = splitAtToken (\t -> isSentenceBreak t || isParagraphBreak t)

-- | Generic version of @splitAt...@.
splitAtToken :: (Token -> Bool) -> [Token] -> [[Token]]
splitAtToken p toks =
    case findIndex p toks of
         Just 0  -> splitAtToken p (tail toks)
         Just ix -> take ix toks : splitAtToken p (drop (ix+1) toks)
         Nothing -> if null toks then [] else [toks]

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

