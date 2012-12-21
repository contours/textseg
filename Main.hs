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
    dss <- NLP.Data.galley2003 "/srv/data/Galley2003" "tdt"

    -- remove the 10th document from each count set
    let trainSet = concatMap (removeIndex 10) dss
    let testSet = map (!! 10) dss

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

    forM_ testSet $ \(Annotated name toks (map (toSentenceMass toks)->refs)) -> do
        let txt = BS.concat (map tokenText toks)
        let ref = head refs
        let s1 = fromLinearMass toks $ textTiling toks
        --let s2 = fromLinearMass toks $ nltkTextTiling (BS.unpack txt)
        let s3 = fromLinearMass toks $ topicTiling 2 lda toks
        let s4 = fromLinearMass toks $ sentence_docsim lda toks
        let prn = show . map toInteger
        printf "------------- %s\n" name
        printf "Reference:      %s\n" (prn ref)
        printf "TextTiling:     %s\n" (prn s1)
        --printf "TextTilingNLTK: %s\n" (prn s2)
        printf "TopicTiling:    %s\n" (prn s3)
        printf "JS-divergence:  %s\n" (prn s4)
        printf "Mean Pk for TextTiling:     %.4f\n" (mean (map (pk s1) refs) :: Double)
        --printf "Mean Pk for TextTilingNLTK: %.4f\n" (mean (map (pk s2) refs))
        printf "Mean Pk for TopicTiling:    %.4f\n" (mean (map (pk s3) refs) :: Double)
        printf "Mean Pk for JS-divergence:  %.4f\n" (mean (map (pk s4) refs) :: Double)
        printf "Mean S for TextTiling:     %.4f\n" (mean (map (similarity s1) refs) :: Double)
        --printf "Mean S for TextTilingNLTK: %.4f\n" (mean (map (similarity s2) refs))
        printf "Mean S for TopicTiling:    %.4f\n" (mean (map (similarity s3) refs) :: Double)
        printf "Mean S for JS-divergence:  %.4f\n" (mean (map (similarity s4) refs) :: Double)
        {-
        printf "Original inter-annotator agreement:   %.4f\n" (agreement_fleiss_kappa refs)
        printf "Agreement change for TextTiling:     %+.4f\n" (-agreement_drop refs s1)
        --printf "Agreement change for TextTilingNLTK: %+.4f\n" (-agreement_drop refs s2)
        printf "Agreement change for TopicTiling:    %+.4f\n" (-agreement_drop refs s3)
        printf "Agreement change for JS-divergence:  %+.4f\n" (-agreement_drop refs s4)
        -}

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

