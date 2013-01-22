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
import Data.Ord
import Data.List

import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.TopicTiling
import NLP.Segmentation.NLTK
import qualified NLP.Segmentation.DP as DP
import NLP.SegEval
import qualified NLP.Data
import           NLP.Data (Annotated(..),Dataset,NamedSegmentation(..),Segmentation)
import Util

main = do
    py_initialize
    args <- getArgs

    --ds <- NLP.Data.stargazer_hearst_1997 "data/stargazer_hearst_1997/article.txt"
    --ds <- NLP.Data.moonstone "data/moonstone_corpus"
    --ds <- NLP.Data.choi "/srv/data/choi/1/3-11"
    ds_contours <- NLP.Data.contours "/srv/data/u-series"
    ds_docsouth <- NLP.Data.contours "/srv/data/docsouth"

    let ds_contours_norybesh = map (\d -> d {
            segmentation = filter (\s -> segname s /= "annotators:rybesh") (segmentation d)}) ds_contours

    let ds_merged = zipWith (\d1 d2 -> Annotated {
            name = name d1,
            document = document d1,
            segmentation = segmentation d1 ++ segmentation d2 })
            (sortBy (comparing name) (filter (\d -> name d `elem` map name ds_contours_norybesh) ds_docsouth))
            (sortBy (comparing name) ds_contours_norybesh)

    putStrLn "**** Contours"
    showDatasetInfo ds_contours_norybesh
    putStrLn "**** Docsouth"
    --showDatasetInfo ds_docsouth
    putStrLn "**** Contours+Docsouth"
    showDatasetInfo ds_merged

    --let trainSet = removeIndex 10 ds
    --let testSet = [ds !! 10]
    let trainSet = []
    let testSet = [ds_contours !! 22]

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
    let methods = 
            [ ("TextTiling", adapt textTiling)
            , ("TopicTiling", adapt (topicTiling 6 lda))
            , ("DP baseline", adapt DP.baseline)
            , ("DP-LDA", adapt (DP.lda lda))
            --, ("TextTilingNLTK", nltkTextTiling)
            --, ("JS-divergence", adapt (sentence_docsim lda))
            ] :: [(String, [Token] -> [SentenceMass])]

    forM_ testSet $ \(Annotated name toks refsegs) -> do
        let refs = map (\(NamedSegmentation _ s) -> toSentenceMass toks s) refsegs
        let txt = BS.concat (map tokenText toks)
        let ref = head refs
        let prn = show . map toInteger
        printf "------------- %s\n" name
        forM_ refs (printf "Reference:      %s\n" . prn)
        forM_ methods $ \(name, segment) -> do
            let s = segment toks
            printf "-- %s\n" name
            printf "Segments      :\t%s\n"   (prn s)
            printf "Mean Pk       :\t%.4f\n" (mean (map (pk s) refs) :: Double)
            printf "Mean S        :\t%.4f\n" (mean (map (similarity s) refs) :: Double)
            --printf "Agreement drop:\t%.4f\n" (agreementDrop [s] [refs])

--agreementDrop :: Integral t => [Segmentation t] -> [[Segmentation t]] -> Double
--agreementDrop hyp refs = agreement_fleiss_kappa refs - agreement_fleiss_kappa (hyp:refs)

showDatasetInfo :: Integral a => [Annotated a] -> IO ()
showDatasetInfo ds = do
    printf "Dataset contains %d document(s): \n" (length ds)
    printf "Overall Multi-Pi agreement: %f\n" (agreement_fleiss_pi [map segseg segs | Annotated _ _ segs <- ds])
    printf "Overall Multi-K  agreement: %f\n" (agreement_fleiss_kappa [map segseg segs | Annotated _ _ segs <- ds])
    printf "Overall Artstein-Poesio bias: %f\n" (artstein_poesio_bias [map segseg segs | Annotated _ _ segs <- ds])
    forM_ ds $ \(Annotated name toks segs) -> do
        printf "%s: %d paragraphs, %d sentences, %d words; %d refs (mean segment count %.1f)\n"
            name
            (fromIntegral $ totalParagraphMass toks :: Int)
            (fromIntegral $ totalSentenceMass toks :: Int)
            (fromIntegral $ totalWordMass toks :: Int)
            (length segs)
            (mean (map (\(NamedSegmentation _ s)->length s) segs) :: Double)
        forM_ segs $ \(NamedSegmentation name seg) -> do
            printf "\t%s: %s\n" name (show (map toInteger seg))
        when (length segs >= 2) $ do
            printf "Multi-Pi agreement: %f\n" (agreement_fleiss_pi [map segseg segs])
            printf "Multi-K  agreement: %f\n" (agreement_fleiss_kappa [map segseg segs])
            printf "Artstein-Poesio bias: %f\n" (artstein_poesio_bias [map segseg segs])

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

