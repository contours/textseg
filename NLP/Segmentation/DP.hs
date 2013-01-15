-- Dynamic Programming based methods.
module NLP.Segmentation.DP
    ( baseline
    , lda
    ) where

import Data.Array
import Data.Graph.Inductive
import Data.List (nub)
import qualified Data.Map as Map
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.HashSet as Set
import System.IO.Unsafe (unsafePerformIO)

import NLP.Tokenizer
import NLP.Segmentation
import qualified NLP.Stemmer
import NLP.Data (stopWords)
import qualified NLP.LDA as LDA

import Debug.Trace

stem = BS.pack . NLP.Stemmer.stem NLP.Stemmer.English . map toLower . BS.unpack

-- | Stem, lowercase, and split into sentences.
preprocess :: [Token] -> [[ByteString]]
preprocess toks = map (map (stem . BS.map toLower . tokenText) . filter ok) (splitAtSentences toks)
    where ok w = isWord w && not (Set.member (BS.map toLower (tokenText w)) stopWords)

baseline :: [Token] -> [SentenceMass]
baseline toks = let
    ppd = preprocess toks
    words = nub (concat ppd)
    wordMap = Map.fromList (zip words [0..])
    -- if you say "map" enough times, it stops sounding like a word
    in baseline' $ map (map (wordMap Map.!)) ppd

-- | Algorithm from "A statistical model for domain-independent text segmentation" (Utiyama & Ishara, 2001).
-- Argument: list of sentences, where each word is an integer word ID starting at 0.
baseline' :: [[Int]] -> [SentenceMass]
baseline' sentences = let
    n = length sentences
    -- m is the number of different words in the document
    m = maximum (concat sentences)
    totalLength = sum (map length sentences)
    -- frequency(w,i) is the number of times word w occurs in sentence i
    frequency w i = length (filter (==w) (sentences!!i))

    -- Construct a graph representing all possible segmentations.
    graph = mkGraph
        -- nodes are positions between sentences ("fenceposts"), with node 0 and node n bookending the document
        [(i, ()) | i <- [0..n]]
        -- edges represent possible segments, spanning at least one sentence
        [(i,j,cost i j) |
            i <- [0..n],
            j <- [i+1..n]]
        :: Gr () Double

    -- cum(w,i) is the number of times word w appears before sentence-gap i
    -- memoized by an array
    cums = array ((0,0),(n,m)) $
        [((0,w), 0) | w <- [0..m]] ++
        [((i,w), cum w (i-1) + frequency w (i-1)) | i <- [1..n], w <- [0..m]]
    cum w i = cums ! (i,w)
    -- total(i) is the number of words before sentence-gap i
    total i = sum (map length (take i sentences))
    -- count(w,i,j) is the number of times word w appears in the segment (i,j)
    count w i j = cum w j - cum w i

    cost i j = log (fromIntegral totalLength) + sum [
        let c = fromIntegral (count k i j)
        in if c > 0 then c * log (fromIntegral (m + total j - total i) / (c+1)) else 0
        | k <- [0..m-1]]

    -- The minimum-cost path from 0 to n is the highest-probability segmentation.
    path = sp 0 n graph

    in
    map SentenceMass $ indicesToMasses (tail (init path)) n

lda :: LDA.Model -> [Token] -> [SentenceMass]
lda model toks = let
    sentences = preprocess toks
    n = length sentences
    m = length (nub (concat sentences))
    totalLength = sum (map length sentences) :: Int

    graph = mkGraph
        [(i, ()) | i <- [0..n]]
        [(i,j,cost i j) |
            i <- [0..n],
            j <- [i+1..n]]
        :: Gr () Double

    ll i j = unsafePerformIO $ LDA.logLikelihood 20 model $ concat (take (j-i) (drop i sentences))
    -- basic log-likelihood plus the prior/penalty term
    cost i j = - ll i j + 3.0 * log (fromIntegral totalLength)

    path = sp 0 n graph

    in
    --graph `traceShow`
    map SentenceMass $ indicesToMasses (tail (init path)) n

