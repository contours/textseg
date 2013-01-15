{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
import qualified Data.ByteString.Char8 as BS 
--import           Data.ByteString.Char8 (ByteString)
import Control.Monad
import Text.Printf
import Python.Interpreter (py_initialize)
import System.Directory (doesFileExist)
import Data.Binary
import System.Environment (getArgs)

import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.TopicTiling
import NLP.Segmentation.NLTK
import qualified NLP.Segmentation.DP as DP
import NLP.SegEval
import qualified NLP.Data
import           NLP.Data (Annotated(..),Dataset)
import Util

-- | Return the mean of the agreement drops (relative to the agreement between reference annotators) when each of the reference annotators is replaced by the one being tested in turn.
-- This requires at least two reference annotations.
agreement_drop :: Integral a => [[a]] -> [a] -> Double
agreement_drop refs x = mean (map (agreement_fleiss_kappa refs -) agreements)
    where agreements = map (agreement_fleiss_kappa.(x:).tail) (cycles refs)

main = do
    py_initialize
    args <- getArgs

    --ds <- NLP.Data.stargazer_hearst_1997 "data/stargazer_hearst_1997/article.txt"
    --ds <- NLP.Data.moonstone "data/moonstone_corpus"
    ds <- NLP.Data.choi "/srv/data/choi/1/3-11"

    let trainSet = removeIndex 10 ds
    let testSet = [ds !! 10]

    printf "Training set contains %d documents.\n" (length trainSet)
    putStrLn "Test set info:"
    showDatasetInfo testSet

    let lda_file = case args of
                        [p] -> p
                        _ -> "/srv/data/lda.model"

    lda <- doesFileExist lda_file >>= \case
        True -> do
            printf "Loading trained LDA model from %s\n" lda_file
            decodeFile lda_file
        False -> do
            printf "Training and saving LDA model to %s\n" lda_file
            printf "(this may take a very long time)\n"
            let lda = trainLDA $ map (\(Annotated _ toks _) -> toks) trainSet
            encodeFile lda_file lda
            return lda

    let adapt f toks = fromLinearMass toks (f toks)
    let methods = [
            ("TextTiling", adapt textTiling),
            --("TextTilingNLTK", nltkTextTiling),
            ("TopicTiling", adapt (topicTiling 2 lda)),
            ("JS-divergence", adapt (sentence_docsim lda)),
            ("DP baseline", adapt DP.baseline),
            ("DP-LDA", adapt (DP.lda lda))
            ] :: [(String, [Token] -> [SentenceMass])]

    forM_ testSet $ \(Annotated name toks (map (toSentenceMass toks)->refs)) -> do
        let txt = BS.concat (map tokenText toks)
        let ref = head refs
        let prn = show . map toInteger
        printf "------------- %s\n" name
        printf "Reference:      %s\n" (prn ref)
        forM_ methods $ \(name, segment) -> do
            let s = segment toks
            printf "Segments of %s:\t%s\n"   name (prn s)
            printf "Mean Pk for %s:\t%.4f\n" name (mean (map (pk s) refs) :: Double)
            printf "Mean S  for %s:\t%.4f\n" name (mean (map (similarity s) refs) :: Double)

showDatasetInfo :: [Annotated a] -> IO ()
showDatasetInfo ds = do
    printf "Dataset contains %d document(s): \n" (length ds)
    forM_ ds $ \(Annotated name toks segs) -> do
        printf "%s: %d paragraphs, %d sentences, %d words; %d refs (mean segment count %.1f)\n"
            name
            (fromIntegral $ totalParagraphMass toks :: Int)
            (fromIntegral $ totalSentenceMass toks :: Int)
            (fromIntegral $ totalWordMass toks :: Int)
            (length segs)
            (mean (map length segs) :: Double)

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

removeIndex :: Int -> [a] -> [a]
removeIndex 0 (x:xs) = xs
removeIndex _ [] = error "removeIndex: not enough elements"
removeIndex i (x:xs) = x : removeIndex (i-1) xs

