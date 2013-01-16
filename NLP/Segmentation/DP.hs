-- Dynamic Programming based methods.
module NLP.Segmentation.DP
    ( baseline
--    , lda
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

import NLP.Tokenizer
import NLP.Segmentation
import qualified NLP.Stemmer
import NLP.Data (stopWords)
import qualified NLP.LDA as LDA
import SkewHeap as PQ

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
    succ i = [(cost i j, j) | j <- [i+1..n]]
    Just path = dijkstra succ (== n) 0

    in
    map SentenceMass $ indicesToMasses (tail (init path)) n

{-
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

    numIterations = 20
    ll i j = unsafePerformIO $ LDA.logLikelihood numIterations model $ concat (take (j-i) (drop i sentences))
    -- basic log-likelihood plus the prior/penalty term
    cost i j = - ll i j + 3.0 * log (fromIntegral totalLength)

    path = sp 0 n graph

    in
    --graph `traceShow`
    map SentenceMass $ indicesToMasses (tail (init path)) n
    -}

-- | A* search. You must provide a state successor function (which also provides the cost to reach each state), a function which identifies goal states, an admissible heuristic, and the initial state. Good monads to use in the result are Maybe (to calculate only the first optimal solution) and List (to calculate all optimal solutions, to all possible goal states).
-- States must be orderable because this algorithm maintains a closed set of visited states.
-- For the same reason, the cost-so-far-plus-heuristic function must be monotonic.
aStarSearch :: (Ord s, MonadPlus m) => (s -> Double) -> (s -> [(Double,s)]) -> (s -> Bool) -> s -> m [s]
aStarSearch heuristic succ isGoal start =
    go Set.empty (PQ.singleton (0 + heuristic start) (0, start, []))
    where go _ queue | PQ.null queue = mzero
          go visited queue = let
              (cost, state, path) = PQ.minValue queue
              visited' = Set.insert state visited
              queue' = PQ.deleteMin queue
              -- When a state is enqueued, its key becomes the cost-so-far, plus the cost to transition to that state, plus the estimate of that state's distance from the goal.
              -- Its value is the new cost-so-far, the state itself, and the path taken to get to it.
              enqueueState q (c,s) = PQ.insert (cost + c + heuristic s) (cost + c, s, state:path) q
              queue'' = foldl enqueueState queue' (succ state)
              in
              if Set.member state visited then
                  go visited queue'
              else if isGoal state then
                  (return $ reverse (state:path)) `mplus`
                  go visited' queue''
              else
                  go visited' queue''

-- Dijkstra's algorithm is A* search with a constant heuristic.
dijkstra = aStarSearch (const 0)

