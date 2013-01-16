{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BS 
import           Data.ByteString.Char8 (ByteString)
import Control.Monad
import Text.Printf
import Python.Interpreter (py_initialize)
import System.Directory (doesFileExist)
import Data.Binary
import System.Environment (getArgs)
import System.Path.Glob
import Data.Char (isAlpha, isUpper)
import Data.List
import Data.Maybe

import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.TopicTiling
import NLP.Segmentation.NLTK
import qualified NLP.Segmentation.DP as DP
import NLP.SegEval
import qualified NLP.Data
import           NLP.Data (Annotated(..),Dataset)
import Util

main = do
    filenames <- glob "/srv/data/U. The*/*/*"
    texts <- mapM BS.readFile filenames
    let lda = trainLDA (map (dropSpeakerNames . map newlineToSentenceBreak . tokenize) texts)
    encodeFile "/srv/data/interviews.model" lda

newlineToSentenceBreak (Whitespace "\n") = SentenceBreak "\n"
newlineToSentenceBreak other = other

-- Drops a line/sentence if all words in it are uppercase and the last punctuation is a colon.
dropSpeakerNames :: [Token] -> [Token]
dropSpeakerNames = concat . filter (not . isSpeakerName) . splitAtToken' (\t -> isSentenceBreak t || t == Whitespace "\n")
    where isSpeakerName toks =
              (isJust (find isPunctuation toks) `implies` (tokenText (last (filter isPunctuation toks)) == ":"))
              && all (BS.all (\c -> isAlpha c `implies` isUpper c)) [w | Word w <- toks]

implies :: Bool -> Bool -> Bool
implies p q = if p then q else True

