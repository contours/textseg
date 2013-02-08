{- |
Names in this module are intended to be imported qualified.
-}
{-# LANGUAGE ViewPatterns #-}
module NLP.Segmentation.TextTiling
    ( eval
    , Config(..)
    , Result(..)
    , defaultConfig
) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import Data.Packed
import Numeric.Container
import Numeric.LinearAlgebra.Util
import Data.Maybe
import Data.Char
--import Data.List
import qualified Data.HashSet as Set
import Text.Show.Functions ()

import Util (mean,stdev,window)
import NLP.Tokenizer
import NLP.Stemmer
import NLP.FrequencyVector
import NLP.Segmentation
import NLP.Data (stopWords)

data Config = Config
    { w :: Int {-^ Length of pseudosentences. -}
    , k :: Int {-^ Number of pseudosentences on each side of the gap. -}
    , threshold_multiplier :: Double {-^ The threshold is the mean valley depth plus this many standard deviations. -}
    , smoothing_rounds :: Int {-^ How many times to apply the smoothing filter. -}
    , smoothing_radius :: Int {-^ Size of the gap score smoothing filter. -}
    , isStopWord :: ByteString -> Bool
    } deriving (Show)

data Result = Result
    { masses :: [WordMass]
    -- | Coherence score across pseudosentence gaps.
    , gapScores :: [Double]
    -- | 0-based word index of where each pseudosentence gap occurs.
    , gapIndices :: [Int] }

-- TODO: drop hmatrix interface, use Data.Vector directly.

-- TODO: add other configuration items:
--  * stemming enabled
--  * use top-N valleys, or use threshold: high or low

defaultConfig = Config
    { w = 20
    , k = 10
    , threshold_multiplier = -0.5
    , smoothing_rounds = 1
    , smoothing_radius = 1
    , isStopWord = \w -> Set.member w stopWords
    }

-- TODO: allow desired number of segments to be given.
eval :: Config -> [Token] -> Result
eval config text = let
    -- Lowercase, remove stop words, and stem, but keep
    -- the original word-index of each word.
    words :: [(Int, ByteString)]
    words = [(i,stem w) | (i, Word (BS.map toLower->w)) <- zip [0..] (filter isWord text)
                        , not (isStopWord config w)]
        where stem = BS.pack . NLP.Stemmer.stem English . BS.unpack
    totalWordMass = length (filter isWord text)
    -- Group words into pseudo-sentences of length w
    merge :: [(Int, ByteString)] -> (Int, [ByteString])
    merge = foldr (\(i,w) (_,ws) -> (i, w:ws)) (0,[])
    merge' = foldr (\(i,ws) (_,wss) -> (i, ws++wss)) (0,[])
    -- [(index, [word])]
    psentences :: [(Int, [ByteString])]
    psentences = map merge (window (w config) (w config) words)
    -- Compute lexical cohesion score across each gap between pseudo-sentences.
    -- Use a block of k=10 pseudo-sentences on each side of the gap.
    (gapIndices, fromList -> gapCohesionScores) = unzip $
        map (uncurry score . splitAt (k config)) (window (2*k config) 1 psentences)
    dict = mkDictionary (map snd words)
    score (merge'->(_,lws)) (merge'->(i,rws)) =
        (i, cosineSimilarity (frequencyVector dict lws) (frequencyVector dict rws))
    -- Compute a depth score for each gap; this is the distance from the peaks on both sides of a valley, to the valley.
    -- First smooth the score function so we don't get confused by
    -- small valleys. This is done with a mean filter of small arbitrary size.
    numGaps = dim gapCohesionScores
    smoothing_width = 2*smoothing_radius config
    smoothed = iterate (subVector smoothing_width numGaps . conv meanKernel) gapCohesionScores !! smoothing_rounds config
        where meanKernel = constant (1.0/fromIntegral(1+smoothing_width)) (1+smoothing_width)
    -- As for identifying valleys, the TextTiling paper describes this rather ad-hoc algorithm.
    lpeak i = findPeak (-1) (smoothed@>i) i
    rpeak i = findPeak ( 1) (smoothed@>i) i
    findPeak dir x i | i == 0 && dir == -1 = max x (smoothed@>i)
    findPeak dir x i | i == numGaps-1 && dir == 1 = max x (smoothed@>i)
    findPeak dir x i = if smoothed@>i >= x
                          then findPeak dir (smoothed@>i) (i+dir)
                          else x
    isLocalMinimum i | i == 0 = False
    isLocalMinimum i | i == numGaps-1 = False
    isLocalMinimum i = case (compare (smoothed@>(i-1)) (smoothed@>i), compare (smoothed@>i) (smoothed@>(i+1))) of
                            (GT,LT) -> True
                            (EQ,LT) -> True
                            (GT,EQ) -> True
                            _ -> False
    gapDepths = buildVector numGaps $ \i ->
        if isLocalMinimum i
           then lpeak i + rpeak i - 2*(smoothed@>i)
           else 0
    numValleys = length (filter (>0) (toList gapDepths))
    valleyDepths = filter (>0) (toList gapDepths)
    -- Assign boundaries at any valley deeper than a cutoff threshold.
    threshold = if length valleyDepths == 0
                   then 0
                   else mean valleyDepths + threshold_multiplier config * stdev valleyDepths
    boundaries1 = catMaybes $ zipWith assign gapIndices (toList gapDepths)
        where assign i score = if score > threshold then Just i else Nothing
    -- Remove boundaries too near each other.
    -- This heuristic is not described in the original paper, but is present in the NLTK implementation.
    boundaries = concatMap (\[a,b] -> if abs (a-b) < (4*w config) then [a] else [a,b]) (window 2 2 boundaries1)
    masses = case numValleys of
                  0 -> [WordMass totalWordMass]
                  -- convert boundary indices to a list of word masses
                  _ -> map WordMass $ zipWith (-) (boundaries++[totalWordMass]) (0:boundaries++[totalWordMass])
    in Result
        { masses = masses
        , gapScores = toList smoothed
        , gapIndices = gapIndices }
    {-
    if length psentences < 2
       -- Text is too short. Return one segment.
       then [WordMass totalWordMass]
       else showDebugInfo `seq` masses
       -}

