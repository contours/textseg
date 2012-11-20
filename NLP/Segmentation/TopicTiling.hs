{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
module NLP.Segmentation.TopicTiling
    ( trainLDA
    , topicTiling
    ) where

import Control.Monad.ST.Safe
import NLP.SwiftLDA
import qualified Data.Vector.Generic as V
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import Control.Monad

import NLP.Segmentation
import NLP.Tokenizer

import Debug.Trace

data Model = Model WordMap Finalized

type WordMap = HashMap Token Int

-- | Train the LDA classifier on a set of documents.
trainLDA :: [[Token]] -> Model
trainLDA documents = runST $ do
    -- use default random seed
    let seed = V.empty
    -- parameters as in (Riedl 2012)
    let num_topics = 100
    -- XXX: 'a' may be scaled by 'k', check the SwiftLDA source code
    let a = 50
    let b = 0.01
    let e = Nothing -- learning rate exponent. TODO: need to set this?
    lda <- initial seed num_topics a b e
    let num_iter = 500
    let wordMap = mkWordMap (concat documents)
    let docs0 = V.fromList (toDocs wordMap documents)
    docs <- foldM (\docs t -> pass t lda docs) docs0 [0..num_iter-1]
    final <- finalize lda
    return (Model wordMap final)

mkWordMap :: [Token] -> WordMap
mkWordMap toks = fst $ foldr fn (M.empty,0) (filter isWord toks)
    where fn w (map,n) = if M.member w map
                            then (map,n)
                            else (M.insert w n map, n+1)

toDocs :: WordMap -> [[Token]] -> [Doc]
toDocs wordMap documents = zipWith (toDoc wordMap) [0..] documents

toDoc :: WordMap -> Int -> [Token] -> Doc
toDoc wordMap i toks = (i, toWordVec toks)
    where toWordVec toks = V.fromList [(wordMap M.! w, Nothing) | w <- toks, isWord w]


topicTiling :: Model -> [Token] -> [SentenceMass]
topicTiling (Model wordMap lda) toks =
    let 
    docTopicWeights lda (toDoc wordMap 0 toks)
    `traceShow`
    undefined

