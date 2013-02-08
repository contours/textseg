{- |
Names in this module are intended to be imported qualified.
-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE FlexibleContexts #-}
module NLP.Segmentation.TopicTiling
    ( train
    , eval
    , TrainConfig(..)
    , defaultTrainConfig
    , Config(..)
    , defaultConfig
    , Result(..)
    , LDA.Model
    ) where

import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))
import Data.List
import Numeric.GSL.Statistics (mean,stddev)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Storable as Storable
import Data.Char (toLower)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Random
import Data.Array.IArray (elems)

import NLP.Segmentation
import NLP.Tokenizer
import NLP.FrequencyVector
import qualified NLP.LDA as LDA
import qualified NLP.GibbsLDA as GibbsLDA
import Util (window)

-- TODO: drop hmatrix interface, use Data.Vector directly.
-- TODO: add other configuration items:
--  * stemming/stopwords
--  * use top-N valleys, or use threshold
--  * random generator (pass through to NLP.LDA)

data TrainConfig = TrainConfig
    { train_iterations :: Int
    , num_topics :: Int
    , alpha :: Double
    , beta :: Double }
    deriving Show

defaultTrainConfig :: TrainConfig
defaultTrainConfig = TrainConfig
    { train_iterations = 500
    , num_topics = 100
    , alpha = 50.0 / fromIntegral (num_topics defaultTrainConfig)
    , beta = 0.01 }

data Config = Config
    { model :: LDA.Model
    , infer_iterations :: Int
    -- | @w@ is a sentence windowing parameter, and should be set based on the expected length of segments. Depends on the data set, strongly affects results.
    , w :: Int
    -- | @threshold_multiplier@ is how many standard deviations to add to the mean valley depth when computing the gap threshold. Recommended values are (-1) or (+1).
    , threshold_multiplier :: Double }

defaultConfig :: LDA.Model -> Config
defaultConfig model = Config
    { model = model
    , infer_iterations = 100
    , w = 3
    , threshold_multiplier = -1.0 }

data Result = Result
    { masses :: [SentenceMass]
    -- | Assignment of a topic to each Word token.
    -- Unknown words have an arbitrary negative topic id.
    , wordTopics :: [Int]
    -- | Topic coherence score over sentence gaps.
    , gapScores :: [Double]
    }

-- | Train the LDA classifier on a set of documents.
train :: TrainConfig -> [[Token]] -> LDA.Model
train config documents = unsafePerformIO $ GibbsLDA.estimate
    (alpha config)
    (beta config)
    (num_topics config)
    (train_iterations config)
    [[w | Word (BS.map toLower->w) <- doc] | doc <- documents]

infer :: Config -> [[ByteString]] -> [Int]
-- Infer word topic, sentence-wise (each sentence is considered a separate document).
-- Passing the True flag to infer enables returning the most common assignment, rather than the last.
-- FIXME: chain RNGs together instead of using global here. also keep in mind, getStdRandom is a mutex when parallelizing
infer config sentenceWords = unsafePerformIO $ do
    model' <- getStdRandom (LDA.infer (infer_iterations config) True (model config) sentenceWords)
    return $ concatMap elems $ LDA.tassign model'

eval :: Config -> [Token] -> Result
eval config text = let
    wordMap = LDA.wordmap (model config)
    -- Lowercase all words, and drop unknown words.
    -- Keep the original word-index of each word.
    wordsOf s = [(i, w) | (i, Word (BS.map toLower->w)) <- zip [0..] (filter isWord s), Map.member w wordMap]
    sentenceWords = map wordsOf (splitAtSentences text)
    -- wordTopics is the final assignment of a topic to each word in sequence.
    -- This does not include the removed stop/unknown words.
    wordTopics0 :: [Int]
    wordTopics0 = infer config (map (map snd) sentenceWords)
    -- Insert the missing stop/unknown words, assigning the topic (-1) to them.
    stopTopic = (-1)
    wordTopics = V.replicate (fromIntegral (totalWordMass text)) stopTopic V.// [(index,topic) | ((index,_),topic) <- zip (wordsOf text) wordTopics0]
    sentences :: [Vector Int]
    sentences = takesV (sentenceWordMass text) wordTopics
    -- Represent each sentence as a topic frequency vector and compute a distance metric over sentence gaps.
    -- But set the stop-word topic to zero everywhere, since we want to ignore stop words.
    topicDict = mkDictionary (V.toList wordTopics)
    freq :: Vector Int -> Vector Double
    freq x = frequencyVector' topicDict x V.// [(topicDict Map.! stopTopic, 0)]
    metric a b = cosineSimilarity (freq a) (freq b)
    gapScores :: Vector Double
    gapScores = V.fromList [metric (V.concat l) (V.concat r) | (l,r) <- map (splitAt (w config)) (window (2*w config) 1 sentences)]
    -- real sentence index of the i'th gap (gap is on the left of the specified sentence)
    gapIndex i = i + w config

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
    threshold = mean valleyDepths + threshold_multiplier config * stddev valleyDepths
    boundaries = catMaybes $ zipWith assign [0..] (V.toList gapDepths)
        where assign i score = if score > threshold then Just (gapIndex i) else Nothing
    SentenceMass total = totalSentenceMass text
    masses = map SentenceMass $ indicesToMasses boundaries total
    in Result
        { masses = masses
        , wordTopics = V.toList wordTopics
        , gapScores = V.toList gapScores }

takesV :: (Integral i, V.Vector v a) => [i] -> v a -> [v a]
takesV is xs = snd $ mapAccumL (\a b -> swap (V.splitAt (fromIntegral b) a)) xs is
    where swap (a,b) = (b,a)

{-
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
-}

