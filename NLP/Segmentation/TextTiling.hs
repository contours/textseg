{-# LANGUAGE ViewPatterns #-}
module NLP.Segmentation.TextTiling
    ( textTiling
) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import System.IO.Unsafe
import Data.Packed
import Numeric.Container
import Numeric.LinearAlgebra.Util
import Numeric.GSL.Statistics (mean,stddev)
import Data.Maybe
import Data.Char
--import Data.List
import qualified Data.HashSet as Set
import           Data.HashSet (HashSet)
--import Debug.Trace

-- TODO: drop hmatrix interface, use Data.Vector directly.
-- TODO: add optional configuration items:
--  * stop words list
--  * stemming enabled
--  * numeric parameters: w, k, smoothing radius
--  * use top-N valleys, or use threshold: high or low

import Util hiding (mean)
import NLP.Tokenizer
import NLP.Stemmer
import NLP.FrequencyVector
import NLP.Segmentation

stopWords :: HashSet ByteString
--stopWords = Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/jarmasz_szpakowicz_2003.list"
stopWords = Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/nltk_english_stopwords"

textTiling :: [Token] -> [WordMass]
textTiling text = let
    -- Lowercase, remove stop words, and stem, but keep
    -- the original word-index of each word.
    words :: [(Int, ByteString)]
    words = [(i,stem w) | (i, Word (BS.map toLower->w)) <- zip [0..] (filter isWord text)
                        , not (Set.member w stopWords)]
        where stem = BS.pack . NLP.Stemmer.stem English . BS.unpack
    totalWordMass = length (filter isWord text)
    -- Group words into pseudo-sentences of length w=20
    w = 20
    merge :: [(Int, ByteString)] -> (Int, [ByteString])
    merge = foldr (\(i,w) (_,ws) -> (i, w:ws)) (0,[])
    merge' = foldr (\(i,ws) (_,wss) -> (i, ws++wss)) (0,[])
    -- [(index, [word])]
    psentences :: [(Int, [ByteString])]
    psentences = map merge (window w w words)
    -- Compute lexical cohesion score across each gap between pseudo-sentences.
    -- Use a block of k=10 pseudo-sentences on each side of the gap.
    k = 10
    (gapIndices, fromList -> gapCohesionScores) = unzip $
        map (uncurry score . splitAt k) (window (2*k) 1 psentences)
    dict = mkDictionary (map snd words)
    score (merge'->(_,lws)) (merge'->(i,rws)) =
        (i, cosineSimilarity dict lws rws)
    -- Compute a depth score for each gap; this is the distance from the peaks on both sides of a valley, to the valley.
    -- First smooth the score function so we don't get confused by
    -- small valleys. This is done with a mean filter of small arbitrary size.
    radius = 2
    numGaps = dim gapCohesionScores
    smoothed = subVector radius numGaps (conv meanKernel gapCohesionScores)
        where meanKernel = constant (1.0/fromIntegral(radius+1)) (radius+1)
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
    valleyDepths = fromList $ filter (>0) (toList gapDepths)
    -- Assign boundaries at any valley deeper than a cutoff threshold.
    -- Threshold is one standard deviation deeper than the mean valley depth.
    threshold = mean valleyDepths - stddev valleyDepths
    boundaries1 = catMaybes $ zipWith assign gapIndices (toList gapDepths)
        where assign i score = if score > threshold then Just i else Nothing
    -- Remove boundaries too near each other.
    -- This heuristic is not described in the original paper, but is present in the NLTK implementation.
    boundaries = concatMap (\[a,b] -> if abs (a-b) < (4*w) then [a] else [a,b]) (window 2 2 boundaries1)
    in 
    case numValleys of
         0 -> [WordMass totalWordMass]
         -- convert boundary indices to a list of word masses
         _ -> map WordMass $ zipWith (-) (boundaries++[totalWordMass]) (0:boundaries++[totalWordMass])

