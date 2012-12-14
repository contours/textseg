{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module NLP.Segmentation.TopicTiling
    ( trainLDA
    , topicTiling
    , Model
    ) where

import NLP.SwiftLDA
import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import Data.List
import Numeric.GSL.Statistics (mean,stddev)
import Data.Vector.Unboxed (Vector)
import Control.Monad
import Control.Monad.ST

import NLP.Segmentation
import NLP.Tokenizer
import NLP.FrequencyVector

data Model = Model WordMap Finalized
type WordMap = HashMap Token Int

-- | Train the LDA classifier on a set of documents.
trainLDA :: [[Token]] -> Model
trainLDA documents = runST $ do
    -- suggested parameters as in Riedl 2012
    let num_topics = 100
        -- XXX: 'a' may be scaled by 'k' (?), check the LDA source code
        a = 50
        b = 0.01
        num_iter = 500
        wordMap = mkWordMap (concat documents)
        docs0 = V.fromList (toDocs wordMap documents)
    -- using an arbitrary random seed
    lda <- initial V.empty num_topics a b Nothing
    runLDA num_iter lda docs0
    f <- finalize lda
    return $ Model wordMap f

runLDA n lda docs = foldM (\_ t -> pass t lda docs) undefined [1..n]
runLDA1 n lda doc = foldM (\_ t -> passOne t lda doc) undefined [1..n]

mkWordMap :: [Token] -> WordMap
mkWordMap toks = fst $ foldr fn (M.empty,0) (filter isWord toks)
    where fn w (map,n) = if M.member w map
                            then (map,n)
                            else (M.insert w n map, n+1)

toDocs :: WordMap -> [[Token]] -> [Doc]
toDocs wordMap documents = zipWith (toDoc wordMap) [0..] documents

toDoc :: WordMap -> Int {-^ document id -} -> [Token] -> Doc
toDoc wordMap i toks = (i, toWordVec toks)
    -- TODO: handle words which are not in the map
    where toWordVec toks = V.fromList [(wordMap M.! w, Nothing) | w <- toks, isWord w]

takesV :: (Integral i, V.Vector v a) => [i] -> v a -> [v a]
takesV is xs = snd $ mapAccumL (\a b -> swap (V.splitAt (fromIntegral b) a)) xs is
    where swap (a,b) = (b,a)

topicTiling :: Model -> [Token] -> [SentenceMass]
topicTiling (Model wordMap lda) toks = let
    num_iter = 100
    -- get per-word topic assignments
    doc :: Vector (W, Maybe Z)
    (_,doc) = runST $ do
        -- using an arbitrary random seed
        x <- unfinalize V.empty lda
        -- FIXME: use the most common assignment over all iterations
        runLDA1 num_iter x (toDoc wordMap 0 toks)
    wordTopics = V.map (\(_,Just z) -> z) doc
    -- split into sentences
    sentences = takesV (filter (>0) (sentenceWordMass toks)) wordTopics
    totalMass = fromIntegral (totalSentenceMass toks) :: Int
    -- represent each sentence as a topic frequency vector
    -- and compute cosine similarity over sentence gaps
    topicDict = mkDictionary (V.toList wordTopics)
    gapScores = V.fromList $ map (uncurry (cosineSimilarity' topicDict)) (zipWith (,) sentences (tail sentences)) :: Vector Double

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
    valleyDepths = V.filter (>0) gapDepths
    -- Assign boundaries at any valley deeper than a cutoff threshold.
    -- Threshold is one standard deviation deeper than the mean valley depth.
    threshold = mean valleyDepths - stddev valleyDepths
    boundaries = V.foldr' (\x xs -> case x of 0 -> xs; _ -> x:xs) []
                          (V.imap assign gapDepths)
        where assign i score =
                  if score > threshold
                     then i+1
                     else 0
    in map SentenceMass $
        -- convert boundary indices to a list of word masses
        zipWith (-) (boundaries++[totalMass]) (0:boundaries++[totalMass])

