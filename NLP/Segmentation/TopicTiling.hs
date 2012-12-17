{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module NLP.Segmentation.TopicTiling
    ( trainLDA
    , topicTiling
    , LDA.Model
    ) where

import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import Data.List
import Numeric.GSL.Statistics (mean,stddev)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Storable as Storable
import Control.Monad
import Data.Char (toLower, isAlpha)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative
import qualified Data.HashSet as Set
import           Data.HashSet (HashSet)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import NLP.Segmentation
import NLP.Tokenizer
import NLP.FrequencyVector
import qualified NLP.LDA as LDA
import Util (window)

import NLP.Stemmer

import Debug.Trace

data Model = Model {
    lda :: LDA.Model,
    num_topics :: Int
    }

-- | Train the LDA classifier on a set of documents.
trainLDA :: [[Token]] -> Model
trainLDA documents =
    -- suggested parameters as in Riedl 2012
    let num_topics = 100
        a = 50.0 / fromIntegral num_topics
        b = 0.01
        num_iter = 500
        words doc = [stem' w | Word (BS.map toLower->w) <- doc
                             , not (Set.member w stopWords)]
        lda = unsafePerformIO $ LDA.train a b num_topics num_iter (map words documents)
    in Model {
        lda = lda,
        num_topics = num_topics
        }

-- TODO: move this definition to NLP.Data, maybe. and pick up the duplicate in TextTiling too
stopWords :: HashSet ByteString
--stopWords = Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/jarmasz_szpakowicz_2003.list"
stopWords = Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/nltk_english_stopwords"

stem' :: ByteString -> ByteString
stem' = BS.pack . stem English . BS.unpack

-- | @topicTiling w model text@.
-- @w@ is a sentence windowing parameter, and should be set based on the expected length of segments. Depends on the data set, strongly affects results.
-- TODO: allow desired number of segments to be given.
topicTiling :: Int -> Model -> [Token] -> [SentenceMass]
topicTiling w model text = let
    num_iter = 100
    -- Lowercase, remove stop words, and stem, but keep
    -- the original word-index of each word.
    wordsOf s = [(i, stem' w) | (i, Word (BS.map toLower->w)) <- zip [0..] (filter isWord s)
                              , not (Set.member w stopWords)]
    sentenceWords = map wordsOf (splitAtSentences text)
    allWords = wordsOf text
    -- one inference step, sentence-wise (each sentence is considered a separate document)
    infer1 :: IO (Vector Int)
    infer1 = V.fromList . map snd . concat . LDA.topic_assignments <$>
        LDA.infer 1 (lda model) [map snd ws | ws <- sentenceWords]
    -- all inference steps
    inferN :: IO [Vector Int]
    inferN = replicateM num_iter infer1
    assignments = unsafePerformIO inferN
    -- find the most common assigment (mode) for each word
    assignmentCounts = [vsum (map (\a -> singleton (num_topics model) (a V.! i)) assignments) | i <- [0..length allWords-1]]
    -- wordTopics is the final assignment of a topic to each word in sequence.
    -- This does not include the removed stop words.
    wordTopics0 :: [Int]
    wordTopics0 = map V.maxIndex assignmentCounts
    -- Insert the missing stop words, assigning the topic (-1) to them.
    stopTopic = (-1)
    wordTopics = V.replicate (fromIntegral (totalWordMass text)) (-1) V.// [(index,topic) | ((index,_),topic) <- zip allWords wordTopics0]
    sentences :: [Vector Int]
    sentences = takesV (sentenceWordMass text) wordTopics
    -- Represent each sentence as a topic frequency vector and compute a distance metric over sentence gaps.
    -- But set the stop-word topic to zero everywhere, since we want to ignore stop words.
    topicDict = mkDictionary (V.toList wordTopics)
    freq :: Vector Int -> Vector Double
    freq x = frequencyVector' topicDict x V.// [(topicDict Map.! stopTopic, 0)]
    metric a b = cosineSimilarity (freq a) (freq b)
    gapScores :: Vector Double
    gapScores = V.fromList [metric (V.concat l) (V.concat r) | (l,r) <- map (splitAt w) (window (2*w) 1 sentences)]
    -- real sentence index of the i'th gap (gap is on the left of the specified sentence)
    gapIndex i = i+w

    -- TODO: unify the following process between TopicTiling and TextTiling.

    -- compute depth score for all local minima
    numGaps = V.length gapScores
    lpeak i = findPeak (-1) (gapScores!i) i
    rpeak i = findPeak ( 1) (gapScores!i) i
    findPeak dir x i =
        if | i == 0 && dir == -1 -> max x (gapScores!i)
           | i == numGaps-1 && dir == 1 -> max x (gapScores!i)
           | gapScores!i >= x -> findPeak dir (gapScores!i) (i+dir)
           | otherwise -> x
    isLocalMinimum i | i == 0 = False
    isLocalMinimum i | i == numGaps-1 = False
    isLocalMinimum i = case (compare (gapScores!(i-1)) (gapScores!i), compare (gapScores!i) (gapScores!(i+1))) of
                            (GT,LT) -> True
                            (EQ,LT) -> True
                            (GT,EQ) -> True
                            _ -> False
    gapDepths :: Storable.Vector Double
    gapDepths = V.generate numGaps $ \i ->
        if isLocalMinimum i
           then lpeak i + rpeak i - 2*(gapScores!i)
           else 0
    valleyDepths = V.filter (>0) gapDepths
    -- Assign boundaries at any valley deeper than a cutoff threshold.
    -- Threshold is one standard deviation deeper than the mean valley depth.
    threshold = mean valleyDepths - stddev valleyDepths
    boundaries = catMaybes $ zipWith assign [0..] (V.toList gapDepths)
        where assign i score =
                  if score > threshold
                     then Just (gapIndex i)
                     else Nothing
    SentenceMass total = totalSentenceMass text
    masses = map SentenceMass $ indicesToMasses boundaries total
    in
    (threshold, gapDepths)
    `traceShow`
    masses

vsum :: [Vector Int] -> Vector Int
vsum = foldl1' (V.zipWith (+))

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM _ q [] = return [q]
scanM f q (x:xs) =
   do q2 <- f q x
      qs <- scanM f q2 xs
      return (q:qs)

singleton :: Int -> Int -> Vector Int
singleton l i = V.replicate l 0 V.// [(i,1)]

takesV :: (Integral i, V.Vector v a) => [i] -> v a -> [v a]
takesV is xs = snd $ mapAccumL (\a b -> swap (V.splitAt (fromIntegral b) a)) xs is
    where swap (a,b) = (b,a)

