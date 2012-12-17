{-# LANGUAGE ViewPatterns #-}
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
import Control.Monad
import Data.Char (toLower, isAlpha)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative
import qualified Data.HashSet as Set
import           Data.HashSet (HashSet)
import qualified Data.Map as Map

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
        a = 50
        b = 0.01
        num_iter = 5
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

topicTiling :: Model -> [Token] -> [SentenceMass]
topicTiling model text = let
    num_iter = 100
    -- Lowercase, remove stop words, and stem, but keep
    -- the original word-index of each word.
    words = [(i, stem' w) | (i, Word (BS.map toLower->w)) <- zip [0..] (filter isWord text)
                          , not (Set.member w stopWords)]
    -- one inference step
    -- XXX: should this re-use the output?
    infer1 :: IO (Vector Int)
    infer1 = V.fromList . map snd . head . LDA.topic_assignments <$>
        LDA.infer 1 (lda model) [map snd words]
    -- all inference steps
    inferN :: IO [Vector Int]
    inferN = replicateM num_iter infer1
    assignments = unsafePerformIO inferN
    -- take the most common assigment (mode) for each word
    singleton :: Int -> Int -> Vector Int
    singleton l i = V.replicate l 0 V.// [(i,1)]
    -- wordTopics is the final assignment of a topic to each word in sequence.
    -- This does not include the removed stop words.
    wordTopics0 :: [Int]
    wordTopics0 = map V.maxIndex [vsum (map (\a -> singleton (num_topics model) (a V.! i)) assignments) | i<-[0..length words-1]]
    -- Insert the missing stop words, assigning the topic (-1) to them.
    stopTopic = (-1)
    wordTopics = V.replicate (fromIntegral (totalWordMass text)) (-1) V.// [(index,topic) | ((index,_),topic) <- zip words wordTopics0]
    -- split into sentences, now that the length matches up with the original word masses
    sentences = takesV (filter (>0) (sentenceWordMass text)) wordTopics
    -- Represent each sentence as a topic frequency vector and compute a distance metric over sentence gaps.
    -- But set topic (-1) to zero everywhere, since we want to ignore stop words.
    topicDict = mkDictionary (V.toList wordTopics)
    freq x = frequencyVector' topicDict x V.// [(topicDict Map.! stopTopic, 0)]
    metric a b = cosineSimilarity (freq a) (freq b)
    gapScores = V.fromList $ map (uncurry metric) (zipWith (,) sentences (tail sentences)) :: Vector Double

    -- TODO: unify the following process between TopicTiling and TextTiling.

    -- compute depth score for all local minima
    numGaps = V.length gapScores
    lpeak i = findPeak (-1) (gapScores!i) i
    rpeak i = findPeak ( 1) (gapScores!i) i
    findPeak dir x i | i == 0 && dir == -1 = max x (gapScores!i)
    findPeak dir x i | i == numGaps-1 && dir == 1 = max x (gapScores!i)
    findPeak dir x i = if gapScores!i >= x
                          then findPeak dir (gapScores!i) (i+dir)
                          else x
    isLocalMinimum i | i == 0 = False
    isLocalMinimum i | i == numGaps-1 = False
    isLocalMinimum i = case (compare (gapScores!(i-1)) (gapScores!i), compare (gapScores!i) (gapScores!(i+1))) of
                            (GT,LT) -> True
                            (EQ,LT) -> True
                            (GT,EQ) -> True
                            _ -> False
    gapDepths = V.generate numGaps $ \i ->
        if isLocalMinimum i
           then lpeak i + rpeak i - 2*(gapScores!i)
           else 0
    valleyDepths = V.filter (>0) gapDepths :: Vector Double
    -- Assign boundaries at any valley deeper than a cutoff threshold.
    -- Threshold is one standard deviation deeper than the mean valley depth.
    --threshold = mean valleyDepths - stddev valleyDepths
    -- XXX: Testing what happens if the number of segments is given.
    threshold = reverse (sort (V.toList valleyDepths)) !! 3 :: Double
    boundaries = V.foldr' (\x xs -> case x of 0 -> xs; _ -> x:xs) []
                          (V.imap assign gapDepths)
        where assign i score =
                  if score >= threshold
                     then i+1
                     else 0
    total = fromIntegral (totalSentenceMass text) :: Int
    in map SentenceMass $
        -- convert boundary indices to a list of word masses
        zipWith (-) (boundaries++[total]) (0:boundaries++[total])

vsum :: [Vector Int] -> Vector Int
vsum = foldl1' (V.zipWith (+))

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f q [] = return [q]
scanM f q (x:xs) =
   do q2 <- f q x
      qs <- scanM f q2 xs
      return (q:qs)

takesV :: (Integral i, V.Vector v a) => [i] -> v a -> [v a]
takesV is xs = snd $ mapAccumL (\a b -> swap (V.splitAt (fromIntegral b) a)) xs is
    where swap (a,b) = (b,a)

