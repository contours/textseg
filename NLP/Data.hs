{-# LANGUAGE OverloadedStrings #-}
module NLP.Data
    ( Segmentation
    , Document
    , Annotated(..)
    , Dataset

    , stargazer_hearst_1997
    , moonstone
    , galley2003_wsj
    , galley2003_wsj_1

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

-- texts are organized by segment count.
galley2003_wsj :: FilePath -> IO [Dataset CharacterMass]
galley2003_wsj path = mapM (galley2003_wsj_1 path) [4,6,8,10,12,14,16,18,20,22]

galley2003_wsj_1 :: FilePath -> Int -> IO (Dataset CharacterMass)
galley2003_wsj_1 path count = do
    let docsPerCount = 50 :: Int
    let txtNames = map (printf "%s/text/wsj/%d/%d.ref" path count) [1..docsPerCount]
    let segNames = map (printf "%s/segments/wsj/%d/%d.ref" path count) [1..docsPerCount]
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
                                  Right is -> indicesToMasses is (BS.length txt)
                                  Left err -> error (printf "%s: %s" name err)
    let masses = map (map CharacterMass) $ zipWith3 parse segs txts segNames
    return (zipWith3 Annotated names docs (map (:[]) masses))

stopWords :: HashSet BS.ByteString
stopWords = Set.unions
    [ Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/nltk_english_stopwords"
    -- A peculiarity of the WSJ dataset -- 's and n't endings are split into a separate word for some reason.
    , Set.fromList ["'s", "n't"]]

