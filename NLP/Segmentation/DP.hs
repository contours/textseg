-- Dynamic Programming based methods.
module NLP.Segmentation.DP
    ( baseline
    ) where

import Data.Array
import Data.Graph.Inductive
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.HashSet as Set

import NLP.Tokenizer
import NLP.Segmentation
import qualified NLP.Stemmer
import NLP.Data (stopWords)

import Debug.Trace

stem = BS.pack . NLP.Stemmer.stem NLP.Stemmer.English . map toLower . BS.unpack

baseline :: [Token] -> [SentenceMass]
baseline toks = let
    ok w = isWord w && not (Set.member (tokenText w) stopWords)
    words = nub (map (stem . tokenText) (filter ok toks))
    wordMap = Map.fromList (zip words [0..])
    in baseline' $ map (map ((wordMap Map.!) . stem . tokenText) . filter ok) (splitAtSentences toks)

-- | Algorithm from "A statistical model for domain-independent text segmentation" (Utiyama & Ishara, 2001).
-- Argument: list of sentences, where each word is an integer word ID starting at 0.
baseline' :: [[Int]] -> [SentenceMass]
baseline' sentences = let
    numWords = sum (map length sentences)
    n = length sentences
    -- m is the number of different words in the document
    m = maximum (concat sentences)
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

    cost i j = log (fromIntegral numWords) + sum [
        let c = fromIntegral (count k i j)
        in if c > 0 then c * log (fromIntegral (m + total j - total i) / (c+1)) else 0
        | k <- [0..m-1]]

    -- The minimum-cost path from 0 to n is the highest-probability segmentation.
    path = sp 0 n graph

    in
    map SentenceMass $ indicesToMasses (tail (init path)) n


