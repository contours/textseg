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

import Datasets (load_ds)

ex1 = do
    ds <- load_ds

    let (descs, funcs) = unzip
            [("Total mass", \segs -> float (sum (head segs)))
            ,("Mean seg count", \segs -> mean (map length segs) :: Double)
            ,("Min seg mass", \segs -> float (minimum (concat segs)))
            ,("Mean seg mass", \segs -> mean (concat segs) :: Double)
            ,("Max seg mass", \segs -> float (maximum (concat segs)))
            ,("1 - WD", \segs -> 1 - mean_pairwise_permuted (windowdiff' (compute_window_size (concat segs))) segs)
            ,("1 - Pk", \segs -> 1 - mean_pairwise_permuted pk segs)
            , s 1 (1,1)
            , s 2 (1,1)
            , s 3 (1,1)
            , s 5 (1,1)
            , s 1 (1,0)
            , s 2 (1,0)
            , s 3 (1,0)
            , s 5 (1,0)
            ]
            where s n (w1,w2) = ( printf "S(n=%d;Ws=%.1f;Wt=%.1f)" (toInteger n) w1 w2
                                , mean_pairwise (similarity' n (w1,w2)))
                  float x = fromIntegral x :: Double
    printf "\"Document\",%s\n" (intercalate "," (map (\d -> "\""++d++"\"") descs))

    forM_ ds $ \(Annotated docname toks segmentations) -> do
        let all_segs = map segseg segmentations
        --let ([segseg->docsouth], map segseg->others) = partition (("annotators:docsouth"==).segname) segmentations

        printf "\"%s\",%s\n" docname (intercalate "," (map (show . ($ all_segs)) funcs))

main = do
    py_initialize
    ex1

