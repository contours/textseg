{-# LANGUAGE LambdaCase #-}
import qualified Data.ByteString.Lazy as BSL
--import           Data.ByteString.Char8 (ByteString)
import Control.Monad
import Text.Printf
--import Python.Interpreter (py_initialize)
import qualified Data.Aeson as Aeson
import Control.Parallel.Strategies
import Debug.Trace

import NLP.Tokenizer
import NLP.Segmentation
import qualified NLP.Segmentation.TextTiling as TextTiling
--import NLP.SegEval
import qualified NLP.Data
import           NLP.Data (Annotated(..),NamedSegmentation(..))
import Util

import Datasets (load_ds)

main = do
    testSet <- load_ds

    let configs =
            [TextTiling.defaultConfig
                { TextTiling.w = w
                , TextTiling.k = k
                , TextTiling.threshold_multiplier = m
                , TextTiling.smoothing_rounds = n
                , TextTiling.smoothing_radius = s }
            | w <- [8,10,12]
            , k <- [6,8..14]
            , m <- [-1.0,-0.75..0.0]
            , n <- [1..2]
            , s <- if n > 0 then [1..2] else []]

    let adapt f toks = fromLinearMass toks (f toks)
    let texttiling config = (
          (printf "TextTiling-w%d-k%d-m%+.2f-n%d-s%d"
           (TextTiling.w config)
           (TextTiling.k config)
           (TextTiling.threshold_multiplier config)
           (TextTiling.smoothing_rounds config)
           (TextTiling.smoothing_radius config)
          ),
          adapt $ TextTiling.masses . TextTiling.eval config)
    let methods = 
            map texttiling configs
            --[ ("TextTiling", adapt (textTiling 1))
            --, ("DP-Baseline", adapt DP.baseline)
            --, ("DP-LDA", adapt (DP.lda lda))
            :: [(String, [Token] -> [SentenceMass])]
    let show_seg s = show (map toInteger s)
    let add_annotation algname (Annotated docname toks refs) hyp =
            Annotated docname toks (NamedSegmentation algname hyp : refs)
    -- NB: Reference granularity is set to SentenceMass here.
    let reference d = map (\(NamedSegmentation _ s) -> toSentenceMass (document d) s) (segmentation d)

    let all_refs = concatMap reference testSet
    printf "Test set segment lengths: mean %f stdev %f\n"
        (mean (concat all_refs) :: Double)
        (stdev (concat all_refs) :: Double)
    
    BSL.writeFile "out-annotations.json" $ Aeson.encode $ NLP.Data.toJsonRep $ testSet
    printf "Non-algorithmic segmentations written to out-annotations.json\n"

    forM_ methods $ \(algname, segment_fn) -> do
        printf "------------- %s\n" algname
        let hyps = [ let s = segment_fn toks
                     in printf "-- %s: %s" docname (show_seg s) `trace` s
                   | Annotated docname toks _ <- testSet]
                   -- NB: run segmentations in parallel
                   `using` parBuffer 4 (evalTraversable rseq)
        let json_file = "out-"++algname++".json"
        BSL.writeFile json_file $ Aeson.encode $ NLP.Data.toJsonRep $
            zipWith (add_annotation algname) testSet hyps
        printf "JSON-encoded segments written to %s\n" json_file

        printf "%s segment lengths: mean %f stdev %f\n"
            algname
            (mean (concat hyps) :: Double)
            (stdev (concat hyps) :: Double)

