{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.ByteString.Char8 as BS 
import           Data.ByteString.Char8 (ByteString)
import Graphics.Plot
import Data.Packed
import Control.Monad
import Control.Applicative
import Text.Printf
import Data.List
import Python.Interpreter (py_initialize)

import Math.Combinatorics
import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.NLTK
import NLP.SegEval
import Util

-- Reference segmentations for stargazer_hearst_1997 dataset
refs = map (map ParagraphMass) $ [
    [2,3,3,1,3,6,3],
    [2,8,2,4,2,3],
    [2,1,2,3,1,3,1,3,2,2,1],
    [2,1,4,1,1,3,1,4,3,1],
    [3,2,4,3,5,4],
    [2,3,4,2,2,5,3],
    [2,3,2,2,3,1,3,2,3]]

main = do
    py_initialize

    -- Compare our TT implementation to NLTK's
    txt <- BS.readFile "data/stargazer_hearst_1997/article.txt"
    let toks = tokenize txt
    printf "Words in the text: %d\n" (length $ filter isWord $ toks)
    let s1 = toParagraphMass toks $ textTiling toks
    let s2 = toParagraphMass toks $ nltkTextTiling (BS.unpack txt)
    printf "TextTiling: %s\n" (show s1)
    printf "TextTilingNLTK: %s\n" (show s2)
    let a = agreement_fleiss_kappa refs
    printf "Original inter-annotator agreement: %f\n" a
    printf "Agreement drop for TextTiling: %f\n" (a - agreement_fleiss_kappa (s1:refs))
    printf "Agreement drop for TextTilingNLTK: %f\n" (a - agreement_fleiss_kappa (s2:refs))

-- | k-fold cross validation.
-- Randomly permuting the data set is up to the caller.
crossValidate :: forall sample model score.
       Int
    -> ([sample] -> model)
    -> (model -> [sample] -> score)
    -> [sample]
    -> [score]
crossValidate k train test samples = map validate folds
    where n = length samples
          nk = div n k
          subsamples :: [[sample]]
          subsamples = take k (window nk nk samples)
          folds :: [([sample],[sample])] -- [(validation,training)]
          folds = map (splitAt nk . concat) (cycles subsamples)
          validate (v,t) = test (train t) v

