{-# LANGUAGE LambdaCase #-}
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Lazy as BSL
--import           Data.ByteString.Char8 (ByteString)
import Control.Monad
import Text.Printf
import Python.Interpreter (py_initialize)
import System.Directory (doesFileExist)
import Data.Binary
import System.Environment (getArgs)
import qualified Data.Aeson as Aeson
import Control.Applicative
import Data.List
import Control.Parallel.Strategies
import Debug.Trace

import NLP.Tokenizer
import NLP.Segmentation
import qualified NLP.Segmentation.TextTiling as TextTiling
import qualified NLP.Segmentation.TopicTiling as TopicTiling
import qualified NLP.Segmentation.DP as DP
import NLP.SegEval
import qualified NLP.Data
import           NLP.Data (Annotated(..),NamedSegmentation(..))
import Util

import Datasets (load_ds)

main = do
    py_initialize
    args <- getArgs
    testSet <- load_ds

    --putStrLn "Test set info:"
    --showDatasetInfo testSet

    let lda_file = case args of
                        [p] -> p
                        _ -> "/srv/data/lda.model"

    lda <- doesFileExist lda_file >>= \case
        True -> do
            printf "Loading trained LDA model from %s\n" lda_file
            decodeFile lda_file
        False -> do
            fail $ printf "Model file does not exist: %s\n" lda_file

    let adapt f toks = fromLinearMass toks (f toks)
    let texttiling x = (printf "TextTiling%+.2f" x, adapt $
            TextTiling.masses . TextTiling.eval TextTiling.defaultConfig { TextTiling.threshold_multiplier = x })
    let topictiling w x = (printf "TopicTiling_%d_%+.2f" w x, adapt $
            TopicTiling.masses . TopicTiling.eval ((TopicTiling.defaultConfig lda) { TopicTiling.w = w }))
    let methods = 
            [topictiling w x | w <- [3..8], x <- [-0.5,-0.25..2.0]::[Double]]
            --map texttiling [-1.0,-0.75..2.0]
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

