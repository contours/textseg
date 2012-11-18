{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenizer
    ( tokenize
    , Token(..)
    , splitAtParagraphs
    , isWord
    , isParagraphBreak
    , wordCount
    ) where

--import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace,take)
import Data.Char (isSpace, isPunctuation)
import Control.Applicative
import Data.List

-- TODO: SentenceBreak
data Token
    = Word !ByteString
    | ParagraphBreak !ByteString
    | Punctuation !ByteString
    | Whitespace !ByteString
    deriving (Eq,Show)

tokenize :: ByteString -> [Token]
tokenize txt = case parseOnly (many token) txt of
                    Left err -> error err
                    Right ts -> ts
    where token = word <|> parBreak <|> whitespace <|> punctuation
          word = Word <$> takeWhile1 isWordChar
          parBreak = ParagraphBreak <$> (string "\n\n" <|> string "\r\n\r\n")
          whitespace = Whitespace <$> takeWhile1 isSpace
          punctuation = Punctuation <$> takeWhile1 (\c -> not (isSpace c) && not (isWordChar c))
          isWordChar = inClass "a-zA-Z0-9_'"

-- | Splits token stream according to ParagraphBreak s, discarding them.
splitAtParagraphs :: [Token] -> [[Token]]
splitAtParagraphs toks =
    case findIndex isParagraphBreak toks of
         Just ix -> take ix toks : splitAtParagraphs (drop (ix+1) toks)
         Nothing -> [toks]

isWord :: Token -> Bool
isWord (Word _) = True
isWord _ = False
isParagraphBreak :: Token -> Bool
isParagraphBreak (ParagraphBreak _) = True
isParagraphBreak _ = False

wordCount :: [Token] -> Int
wordCount = length . filter isWord

