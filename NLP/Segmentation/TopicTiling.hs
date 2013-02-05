{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Text.Printf
import Data.Binary
import System.Random
import Data.Array.IArray (elems)

import NLP.Segmentation
import NLP.Tokenizer
import NLP.FrequencyVector
import qualified NLP.LDA as LDA
import qualified NLP.GibbsLDA as GibbsLDA
import Util (window)

import qualified NLP.Stemmer

import Debug.Trace

--stem = BS.pack . NLP.Stemmer.stem NLP.Stemmer.English . BS.unpack
-- stemming disabled
stem = id

-- | Train the LDA classifier on a set of documents.
trainLDA :: [[Token]] -> LDA.Model
trainLDA documents =
    -- suggested parameters as in Riedl 2012
    let num_topics = 100
        a = 50.0 / fromIntegral num_topics
        b = 0.01
        num_iter = 500
        words doc = [stem w | Word (BS.map toLower->w) <- doc]
                            -- , not (Set.member w stopWords)]
    --in unsafePerformIO $ getStdRandom $ LDA.estimate a b num_topics num_iter (map words documents)
    in unsafePerformIO $ GibbsLDA.estimate a b num_topics num_iter (map words documents)

infer :: (Int, LDA.Model, [[ByteString]]) -> [Int]
-- Infer word topic, sentence-wise (each sentence is considered a separate document).
-- Passing the True flag to infer enables returning the most common assignment, rather than the last.
-- FIXME: chain RNGs together instead of using global here. also keep in mind, getStdRandom can be a barrier when parallelizing
infer (num_iter,model,sentenceWords) = unsafePerformIO $ concatMap elems . LDA.tassign <$> getStdRandom (LDA.infer num_iter True model sentenceWords)

-- | @topicTiling w threshold_multiplier model text@.
-- @w@ is a sentence windowing parameter, and should be set based on the expected length of segments. Depends on the data set, strongly affects results.
-- @threshold_multiplier@ is how many standard deviations to add to the mean valley depth when computing the gap threshold. Recommended values are (-1) or (+1).
-- TODO: allow desired number of segments to be given.
topicTiling :: Int -> Double -> LDA.Model -> [Token] -> [SentenceMass]
topicTiling w threshold_multiplier model text = let
    num_iter = 100
    wordMap = LDA.wordmap model
    -- Lowercase and remove unknown words.
    -- Keep the original word-index of each word.
    wordsOf s = [(i, stem w) | (i, Word (BS.map toLower->w)) <- zip [0..] (filter isWord s)
                             -- , not (Set.member w stopWords)
                             , Map.member w wordMap]
    sentenceWords = map wordsOf (splitAtSentences text)
    words = wordsOf text
    -- wordTopics is the final assignment of a topic to each word in sequence.
    -- This does not include the removed stop words.
    wordTopics0 :: [Int]
    wordTopics0 = infer (num_iter,model,map (map snd) sentenceWords)
    -- Insert the missing stop words, assigning the topic (-1) to them.
    stopTopic = (-1)
    wordTopics = V.replicate (fromIntegral (totalWordMass text)) stopTopic V.// [(index,topic) | ((index,_),topic) <- zip words wordTopics0]
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
    threshold = mean valleyDepths + threshold_multiplier * stddev valleyDepths
    boundaries = catMaybes $ zipWith assign [0..] (V.toList gapDepths)
        where assign i score = if score > threshold then Just (gapIndex i) else Nothing
    SentenceMass total = totalSentenceMass text
    masses = map SentenceMass $ indicesToMasses boundaries total
    showDebugInfo = unsafePerformIO $ do
        printf "# TopicTiling {'threshold': %.4f, 'score': %s, 'w': %d, 'predicted': %s}\n" threshold (show (V.toList gapScores)) w (show (map toInteger masses))
        return ()
    in
    --showDebugInfo `seq`
    masses

-- | Jensen–Shannon divergence, a smoothed and symmetric version of 'klDivergence'.
jsDivergence :: (Eq a, Floating a, V.Vector v a) => v a -> v a -> a
jsDivergence p q = 0.5*klDivergence p m + 0.5*klDivergence q m
    where m = V.map (0.5*) (V.zipWith (+) p q)

-- | Kullback-Leibler divergence, a measure of the distance between two probability distributions.
klDivergence :: (Eq a, Floating a, V.Vector v a) => v a -> v a -> a
klDivergence p q = V.sum $ V.zipWith f p q
    where f a b | a == 0 = 0
                | b == 0 = error "klDivergence p q: undefined where q has zeroes and p doesn't"
                | otherwise = a * log (a/b)

vsum :: [Vector Int] -> Vector Int
vsum = foldl1' (V.zipWith (+))

vnormalize v = V.map (/n) v
    where n = sqrt (V.sum (V.zipWith (*) v v))

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

