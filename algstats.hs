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

import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.TopicTiling
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
    let methods = 
            [ ("TextTiling", adapt textTiling)
            , ("TopicTiling", adapt (topicTiling 8 lda))
            , ("DP-Baseline", adapt DP.baseline)
            , ("DP-LDA", adapt (DP.lda lda))
            ] :: [(String, [Token] -> [SentenceMass])]

    BSL.writeFile "out-annotations.json" $ Aeson.encode $ NLP.Data.toJsonRep $ testSet
    printf "Non-algorithmic segmentations written to out-annotations.json\n"

    let prn = show . map toInteger
    forM_ methods $ \(algname, segment) -> do
        printf "------------- %s\n" algname
        out <- forM testSet $ \(Annotated docname toks refsegs) -> do
            let refs = map (\(NamedSegmentation _ s) -> toSentenceMass toks s) refsegs
            printf "-- %s\n" docname
            forM_ refs (printf "Reference:      %s\n" . prn)
            let s = segment toks
            printf "Segments      :\t%s\n"   (prn s)
            printf "Mean Pk       :\t%.4f\n" (mean (map (pk s) refs) :: Double)
            printf "Mean S        :\t%.4f\n" (mean (map (similarity s) refs) :: Double)
            --printf "Agreement drop:\t%.4f\n" (agreementDrop [s] [refs])
            return $ Annotated docname toks (NamedSegmentation algname s : refsegs)
        let result_file = "out-"++algname++".json"
        BSL.writeFile result_file $ Aeson.encode $ NLP.Data.toJsonRep $ out
        printf "Output written to %s\n" result_file

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

