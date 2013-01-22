{-# LANGUAGE ViewPatterns #-}
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

-- The Contours data set.
load_ds_contours = do
    ds_contours_rybesh <- NLP.Data.contours "/srv/data/u-series"

    -- NB: remove rybesh's segs, since they don't cover all documents in the set
    let ds_contours = map (\d -> d {
            segmentation = filter (\s -> segname s /= "annotators:rybesh") (segmentation d)}) ds_contours_rybesh

    return ds_contours

-- The Contours data set plus the docsouth reference segmentations (only of those documents).
load_ds_merged = do
    ds_contours <- load_ds_contours
    ds_docsouth <- NLP.Data.contours "/srv/data/docsouth"

    -- add docsouth reference segmentations to all present contours documents
    let ds_merged = zipWith (\d1 d2 -> Annotated {
            name = name d1,
            document = document d1,
            segmentation = segmentation d1 ++ segmentation d2 })
            (sortBy (comparing name) (filter (\d -> name d `elem` map name ds_contours) ds_docsouth))
            (sortBy (comparing name) ds_contours)

    return ds_merged

ex1 = do
    ds_merged <- load_ds_merged

    let n_values = [1,2,3,5,8,16,32,48,64] :: [Int]
    let s_header = intercalate "," [printf "S(n=%d)" n | n <- n_values]
    printf "Document,Mean Segment Mass,WD Window Size,1 - Mean WD,%s\n" s_header

    forM_ ds_merged $ \(Annotated docname toks segmentations) -> do
        let all_segs = map segseg segmentations
        let ([segseg->docsouth], map segseg->others) = partition (("annotators:docsouth"==).segname) segmentations

        let mean_segment_mass = mean (concat all_segs) :: Double
        let window_size = round (mean_segment_mass / 2.0) :: Int
        let mean_wd = mean_pairwise (windowdiff' (fromIntegral window_size)) all_segs
        let s_values = [mean_pairwise (similarity' n) all_segs | (fromIntegral->n) <- n_values]

        printf "%s,%f,%d,%f,%s\n" docname mean_segment_mass window_size (1-mean_wd)
                                  (intercalate "," (map show s_values))

main = do
    py_initialize
    ex1

