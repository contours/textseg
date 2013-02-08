{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module NLP.Tokenizer
    ( tokenize
    , simpleTokenize
    , breakContractions
    , breakPunctuation
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
    , isPunctuation
    , wordCount
    ) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace,take)
import Data.Char (isSpace,isAlphaNum)
import qualified Data.Char (isPunctuation)
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

-- | For compatibility. Finds words that end in "n't", "'m", "'s", or similar suffixes, and breaks that suffix off into a separate Word  token.
breakContractions :: [Token] -> [Token]
breakContractions = concatMap f
    where f (Word w) | isSuffix "n't" w = [Word (dropEnd 3 w), Word "n't"]
                     | isSuffix "'m"  w = [Word (dropEnd 2 w), Word "'m"]
                     | isSuffix "'s"  w = [Word (dropEnd 2 w), Word "'s"]
                     | isSuffix "'d"  w = [Word (dropEnd 2 w), Word "'d"]
                     | isSuffix "'re"  w = [Word (dropEnd 3 w), Word "'re"]
                     | isSuffix "'ve"  w = [Word (dropEnd 3 w), Word "'ve"]
                     | isSuffix "'ll"  w = [Word (dropEnd 3 w), Word "'ll"]
          f other = [other]
          isSuffix s xs = not (BS.null xs) && s == BS.drop (BS.length xs - BS.length s) xs
          dropEnd n xs = BS.take (BS.length xs - n) xs

breakPunctuation :: [Token] -> [Token]
breakPunctuation = concatMap f
    where f (Word (BS.unpack->w)) =
              case span punct w of
                   ([], w2) -> g w2
                   (w1, []) -> [Punctuation (BS.pack w1)]
                   (w1, w2) -> Punctuation (BS.pack w1) : g w2
          f other = [other]
          g w = case break punct w of
                     (w1, []) -> [Word (BS.pack w1)]
                     ([], _ ) -> error ("breakPunctuation.g: " ++ w)
                     (w1, w2) -> [Word (BS.pack w1), Punctuation (BS.pack w2)]
          punct c = Data.Char.isPunctuation c || c == '`'

-- For now, simply looks for isolated period characters, question marks, and exclamation marks.
-- Ellipses ("...") may cause false negatives and abbreviations ("U.S.") will cause false positives.
-- TODO: use a more sophisticated algorithm.
createSentenceBreaks :: [Token] -> [Token]
createSentenceBreaks = map f
    where f t = if tokenText t `elem` [".","?","!"]
                   then SentenceBreak (tokenText t)
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
isPunctuation :: Token -> Bool
isPunctuation (Punctuation _) = True
isPunctuation _ = False

wordCount :: [Token] -> Int
wordCount = length . filter isWord

