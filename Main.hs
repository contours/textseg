{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.ByteString.Char8 as BS 
import           Data.ByteString.Char8 (ByteString)
import Control.Monad
import Text.Printf
import Python.Interpreter (py_initialize)

import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.TopicTiling
import NLP.Segmentation.NLTK
import NLP.SegEval
import Util

type Segmentation t = [t]
type Document = [Token]
data Example t = Example Document [Segmentation t]
type Dataset t = [Example t]

stargazer_hearst_1997 :: FilePath -> IO (Dataset ParagraphMass)
stargazer_hearst_1997 articlePath = do
    txt <- BS.readFile articlePath
    let doc = tokenize txt
    let refs = [ [2,3,3,1,3,6,3]
               , [2,8,2,4,2,3]
               , [2,1,2,3,1,3,1,3,2,2,1]
               , [2,1,4,1,1,3,1,4,3,1]
               , [3,2,4,3,5,4]
               , [2,3,4,2,2,5,3]
               , [2,3,2,2,3,1,3,2,3]] :: [[ParagraphMass]]
    return [Example doc refs]

-- | Return the mean of the agreement drops (relative to the agreement between reference annotators) when each of the reference annotators is replaced by the one being tested in turn.
-- This requires at least two reference annotations.
agreement_drop :: Integral a => [[a]] -> [a] -> Double
agreement_drop refs x = mean (map (agreement_fleiss_kappa refs -) agreements)
    where agreements = map (agreement_fleiss_kappa.(x:).tail) (cycles refs)

main = do
    py_initialize

    ds1 <- stargazer_hearst_1997 "data/stargazer_hearst_1997/article.txt"
    showDatasetInfo ds1

    let [Example toks refs] = ds1
    let txt = BS.concat (map tokenText toks)
    let s1 = toParagraphMass toks $ textTiling toks
    let s2 = toParagraphMass toks $ nltkTextTiling (BS.unpack txt)
    let lda = trainLDA [toks]
    let s3 = toParagraphMass toks $ topicTiling lda toks
    printf "TextTiling: %s\n" (show s1)
    printf "TextTilingNLTK: %s\n" (show s2)
    printf "TopicTiling: %s\n" (show s3)
    print $ mean $ map (similarity s1) refs
    print $ mean $ map (similarity s2) refs
    print $ mean $ map (similarity s3) refs
    printf "Original inter-annotator agreement: %.4f\n" (agreement_fleiss_kappa refs)
    printf "Agreement drop for TextTiling: %.4f\n" (agreement_drop refs s1)
    printf "Agreement drop for TextTilingNLTK: %.4f\n" (agreement_drop refs s2)
    printf "Agreement drop for TopicTiling: %.4f\n" (agreement_drop refs s3)

showDatasetInfo :: [Example a] -> IO ()
showDatasetInfo ds = do
    printf "Dataset contains %d document(s): \n" (length ds)
    forM_ ds $ \(Example toks segs) -> do
        printf "%d paragraphs, %d sentences, %d words; %d refs (mean count %.1f)\n" (fromIntegral $ totalParagraphMass toks :: Int) (fromIntegral $ totalSentenceMass toks :: Int) (fromIntegral $ totalWordMass toks :: Int) (length segs) (mean (map length segs) :: Double)

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

