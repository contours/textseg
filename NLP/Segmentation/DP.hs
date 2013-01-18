-- Dynamic Programming based methods.
{-# LANGUAGE FlexibleContexts #-}
module NLP.Segmentation.DP
    ( baseline
    , lda
    ) where

import Data.Array
import Data.List (nub)
import qualified Data.Map as Map
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Control.Applicative

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
    where ok w = isWord w && not (HashSet.member (BS.map toLower (tokenText w)) stopWords)

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
    path = dp cost 0 n

    in
    map SentenceMass $ indicesToMasses (tail (init path)) n

lda :: LDA.Model -> [Token] -> [SentenceMass]
lda model toks = let
    sentences = preprocess toks
    n = length sentences
    m = length (nub (concat sentences))
    totalLength = sum (map length sentences) :: Int

    numIterations = 20
    ll i j = unsafePerformIO $ LDA.logLikelihood numIterations model $ concat (take (j-i) (drop i sentences))
    -- basic log-likelihood plus the prior/penalty term
    cost i j = - ll i j + 3.0 * log (fromIntegral totalLength)

    --path = modifiedDP cost 0 n
    path = modifiedDP (\i j -> let c = cost i j in (i,j,c)`traceShow`c) 0 n

    in
    map SentenceMass $ indicesToMasses (tail (init path)) n

-- | Basic dynamic programming algorithm for segmentation.
-- Requires a cost function and the start and end node IDs.
dp :: (Int -> Int -> Double) -> Int -> Int -> [Int]
-- Hey, Haskell can do imperative programming, too!
dp cost start end = runST $ do
    let infinity = (1/0) :: Double
    minCosts <- newSTUArray (start,end) infinity
    writeArray minCosts start 0
    bp <- newSTUArray (start,end) (-1)
    -- forward pass
    forM_ [start..end] $ \i -> do
        forM_ [i+1..end] $ \j -> do
            ci <- readArray minCosts i
            cj <- readArray minCosts j
            let cj' = ci + cost i j
            when (cj' < cj) $ do
                writeArray minCosts j cj'
                writeArray bp j i
    -- backward pass
    path <- newSTRef [end]
    let trace j = do
        i <- readArray bp j
        modifySTRef' path (i:)
        when (i /= start) (trace i)
    trace end
    readSTRef path

-- | Misra's modification to the DP algorithm for segmentation.
modifiedDP :: (Int -> Int -> Double) -> Int -> Int -> [Int]
modifiedDP cost start end = runST $ do
    let infinity = (1/0) :: Double
    minCosts <- newSTUArray (start,end) infinity
    writeArray minCosts start 0
    bp <- newSTUArray (start,end) (-1)
    -- Keep a count of the number of times a node was "active".
    -- A node is active if it is found to be the best starting point of a segment.
    activity <- newSTUArray (start,end) (0 :: Int)
    lastActiveNode <- newSTRef start
    -- forward pass
    forM_ [start..end] $ \j -> do
        -- Skip begin nodes which are before the last active node.
        active <- readSTRef lastActiveNode
        forM_ [active..j-1] $ \i -> do
            when (i >= active) $ do
                ci <- readArray minCosts i
                cj <- readArray minCosts j
                let cj' = ci + cost i j
                when (cj' < cj) $ do
                    writeArray minCosts j cj'
                    writeArray bp j i
                    -- Node i is active.
                    count <- readArray activity i
                    writeArray activity i (count+1)
                    -- A node is only considered active "for reals" when
                    -- it is active at least twice. For robustness, apparently.
                    when (count >= 1) (writeSTRef lastActiveNode i)
    -- backward pass
    path <- newSTRef [end]
    let trace j = do
        i <- readArray bp j
        modifySTRef' path (i:)
        when (i /= start) (trace i)
    trace end
    readSTRef path

-- A convenient type annotation, specializing newArray to the actual implementation (ST-Unboxed) we use.
newSTUArray :: (MArray (STUArray s) e (ST s), Ix i) => (i, i) -> e -> ST s (STUArray s i e)
newSTUArray = newArray

