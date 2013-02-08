{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Text.Printf
import Control.Parallel.Strategies
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Binary (decodeFile)

import qualified NLP.Segmentation.TopicTiling as TT
import           NLP.Data (Annotated(..))

import Datasets (load_ds)

main = do
    args <- getArgs
    when (length args /= 1) $
        fail "Usage: topictiling-experiment <path to LDA model>"
    lda <- let lda_file = head args
           in doesFileExist lda_file >>= \case
               True -> decodeFile lda_file
               False -> fail $ "Model file does not exist: "++lda_file
    testSet <- load_ds

    let configs =
            [(TT.defaultConfig lda)
                { TT.w = w }
            | w <- [3,4,6,8,10]]
    printf "\"document\",\"w\",\"sentence gap scores\",\"word topic assignments\"\n"
    let csv_lines = [
            printf "\"%s\",%d,\"%s\",\"%s\"\n"
                (name doc)
                (TT.w config)
                (show $ TT.gapScores result)
                (show $ TT.wordTopics result)
            | doc <- testSet
            , config <- configs
            , let result = TT.eval config (document doc)]
            `using` parBuffer 4 rdeepseq
    mapM_ putStr csv_lines

