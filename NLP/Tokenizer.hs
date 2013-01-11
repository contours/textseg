{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenizer
    ( tokenize
    , simpleTokenize
    , Token(..)
    , tokenText
    , splitAtParagraphs
    , splitAtSentences
    , splitAtSentences'
    , splitAtToken
    , splitAtToken'
    , isWord
    , isParagraphBreak
    , isSentenceBreak
    , wordCount
    ) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace,take)
import Data.Char (isSpace,isAlphaNum)
import Control.Applicative
import Data.List
import Data.Hashable (Hashable(..))

data Token
    = Word !ByteString
    | ParagraphBreak !ByteString
    | SentenceBreak !ByteString
    | Punctuation !ByteString
    | Whitespace !ByteString
    deriving (Eq,Show,Ord)

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
          word = do
              cs <- takeWhile1 isWordChar
              if not (BS.any (inClass "a-zA-Z0-9") cs)
                 then return (Punctuation cs)
                 else return (Word cs)
          isWordChar = inClass "a-zA-Z0-9_'"
          parBreak = ParagraphBreak . BS.concat <$> many1 (string "\n\n" <|> string "\r\n\r\n")
          whitespace = Whitespace <$> takeWhile1 isSpace
          punctuation = Punctuation <$> takeWhile1 (\c -> not (isSpace c) && not (isWordChar c))

-- | Tokenizes a simpler type of document.
-- Each line is assumed to be a sentence. All words and punctuation are assumed to be separated by whitespace.
-- Paragraph breaks are not generated.
simpleTokenize :: ByteString -> [Token]
simpleTokenize = intercalate [SentenceBreak "\n"] . map sentence . BS.lines
    where sentence = intersperse (Whitespace " ") . map word . BS.words
          word txt = if BS.any isAlphaNum txt
                        then Word txt
                        else Punctuation txt

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

-- | Splits token stream according to 'SentenceBreak's, keeping the break at the end of each segment.
-- 'ParagraphBreak's are also taken into account as being implicit 'SentenceBreak's.
splitAtSentences' :: [Token] -> [[Token]]
splitAtSentences' = splitAtToken' (\t -> isSentenceBreak t || isParagraphBreak t)

-- | Splits a token stream by the given predicate, discarding matching elements.
splitAtToken :: (Token -> Bool) -> [Token] -> [[Token]]
splitAtToken p toks =
    case findIndex p toks of
         Just 0  -> splitAtToken p (tail toks)
         Just ix -> take ix toks : splitAtToken p (drop (ix+1) toks)
         Nothing -> if null toks then [] else [toks]

-- | Splits a token stream by the given predicate, keeping the matching element at the end of each segment.
splitAtToken' :: (Token -> Bool) -> [Token] -> [[Token]]
splitAtToken' p toks =
    case findIndex p toks of
         Just 0  -> splitAtToken' p (tail toks)
         Just ix -> take (ix+1) toks : splitAtToken' p (drop (ix+1) toks)
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

