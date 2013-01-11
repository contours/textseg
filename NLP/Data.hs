{-# LANGUAGE OverloadedStrings #-}
module NLP.Data
    ( Segmentation
    , Document
    , Annotated(..)
    , Dataset

    , stargazer_hearst_1997
    , moonstone
    , galley2003
    , galley2003_1
    , choi

    , stopWords
    ) where

import qualified Data.ByteString.Char8 as BS 
--import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List (transpose,group)
import Text.Printf (printf)
import qualified Data.HashSet as Set
import           Data.HashSet (HashSet)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (assert)
import System.Path.Glob (glob)
import System.FilePath (combine)

import NLP.Tokenizer
import NLP.Segmentation

--import Debug.Trace

type Segmentation t = [t]
type Document = [Token]
type Dataset t = [Annotated t]
data Annotated t = Annotated {
    name :: String,
    document :: Document,
    segmentation :: [Segmentation t]
    }

stargazer_hearst_1997 :: FilePath -> IO (Dataset ParagraphMass)
stargazer_hearst_1997 articlePath = do
    txt <- BS.readFile articlePath
    let doc = tokenize txt
    let refs = [ [2,3,3,1,3,6,3]
               , [2,8,2,4,2,3]
               , [2,1,2,3,1,3,1,3,2,2,1]
               , [2,1,4,1,1,3,1,4,3,1]
               , [3,2,4,3,5,4]
               , [2,3,4,2,2,5,3]
               , [2,3,2,2,3,1,3,2,3]] :: [[ParagraphMass]]
    return [Annotated articlePath doc refs]

moonstone :: FilePath -> IO (Dataset ParagraphMass)
moonstone path = do
    let chapters = [1..5]++[7..21] :: [Int]
    let csvNames = map (printf "%s/segmentations/ch%d.csv" path) chapters
    let txtNames = map (printf "%s/text/ch%d.txt" path) chapters
    txts <- mapM BS.readFile txtNames
    csvs <- mapM BS.readFile csvNames
    let docs = map tokenize txts
    let segs = map (\(csv,name) -> case parseOnly moonstone_segmentation csv of
                                        Right seg -> seg
                                        Left err -> error (printf "%s: %s" name err))
                   (zip csvs csvNames)
    return (zipWith3 Annotated txtNames docs segs)

-- | Parses a .csv file from the Moonstone dataset into a list of paragraph-mass segmentations.
moonstone_segmentation :: Parser [[ParagraphMass]]
moonstone_segmentation = do
    skipWhile (/='\n') >> skipSpace -- skip entire header line
    paras <- sepBy1 (tail <$> sepBy1 decimal (char ',')) endOfLine
    return $! map countRuns (transpose paras)
        where countRuns = map (ParagraphMass . length) . group

-- | Texts are organized by segment count.
-- @galley2003 path subset@
-- Subset is either "wsj" or "tdt".
galley2003 :: FilePath -> String -> IO [Dataset CharacterMass]
galley2003 path subset = mapM (galley2003_1 path subset) [4,6,8,10,12,14,16,18,20,22]

data Galley2003_Subset = Galley2003_TDT | Galley2003_WSJ

-- | @galley2003_1 path subset count@
-- Subset is either "wsj" or "tdt".
galley2003_1 :: FilePath -> String -> Int -> IO (Dataset CharacterMass)
galley2003_1 path subset count = do
    let docsPerCount = 50 :: Int
    let txtNames = map (printf "%s/text/%s/%d/%d.ref" path subset count) [1..docsPerCount]
    let segNames = map (printf "%s/segments/%s/%d/%d.ref" path subset count) [1..docsPerCount]
    let names = txtNames
    txts <- mapM BS.readFile txtNames
    segs <- mapM BS.readFile segNames
    -- In this dataset, each sentence is a line.
    -- Remove sentence breaks unless they are followed by a newline.
    let fixSentenceBreaks (f : Whitespace w : xs) =
            case (f, BS.elem '\n' w) of
                 (Word s, True) -> Word s : SentenceBreak w : fixSentenceBreaks xs
                 (SentenceBreak s, True) -> SentenceBreak (BS.append s w) : fixSentenceBreaks xs
                 (Punctuation s, True) -> SentenceBreak (BS.append s w) : fixSentenceBreaks xs
                 (Whitespace s, True) -> SentenceBreak (BS.append s w) : fixSentenceBreaks xs
                 -- there shouldn't be any paragraph breaks, but just in case.
                 (ParagraphBreak s, True) -> ParagraphBreak (BS.append s w) : fixSentenceBreaks xs
                 -- no newline? no sentence break.
                 (SentenceBreak s, False) -> Punctuation s : Whitespace w : fixSentenceBreaks xs
                 -- otherwise leave things unaltered
                 (_, False) -> f : fixSentenceBreaks (Whitespace w : xs)
        -- other sentence breaks get removed as well
        fixSentenceBreaks (SentenceBreak s : xs) = Punctuation s : fixSentenceBreaks xs
        fixSentenceBreaks (other : xs) = other : fixSentenceBreaks xs
        fixSentenceBreaks [] = []
    -- clean up the break left over when there's a trailing newline at the end
    let removeLastBreak xs = if isSentenceBreak (last xs) then init xs else xs
    let docs = map (removeLastBreak . fixSentenceBreaks . tokenize) txts
    let parse seg txt name = case parseOnly (decimal `sepBy` skipSpace) seg of
                                  Right is ->
                                      if and ['\n' == BS.index txt (i-1) | i<-is]
                                         then indicesToMasses is (BS.length txt)
                                         else error (printf "%s: not all segment boundaries correspond to newlines" name)
                                  Left err -> error (printf "%s: %s" name err)
    let masses = map (map CharacterMass) $ zipWith3 parse segs txts segNames
    return (zipWith3 Annotated names docs (map (:[]) masses))

-- | Accepts a path containing *.ref files, such as "/home/you/data/choi/2/6-8"
choi :: FilePath -> IO (Dataset SentenceMass)
choi path = do
    names <- glob (combine path "*.ref")
    txts <- mapM BS.readFile names
    let f = splitAtToken (\t -> tokenText t == "==========") . simpleTokenize
    -- 'init' drops the extra trailing sentence break from each segment
    let doc = concat . map init . f
    let seg = (:[]) . map (SentenceMass . length . drop 1 . filter isSentenceBreak) . f
    return (zipWith3 Annotated names (map doc txts) (map seg txts))

stopWords :: HashSet BS.ByteString
stopWords = Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/choi_stopwords.list"
{-
    Set.unions
    [ Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/nltk_english_stopwords"
    -- A peculiarity of the WSJ dataset -- 's and n't endings are split into a separate word for some reason.
    , Set.fromList ["'s", "n't"]]
    -}

