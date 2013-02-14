{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Text.Printf
import Control.Parallel.Strategies
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Binary (decodeFile)

import qualified NLP.Segmentation.DP as DP
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

    let config = DP.Config { DP.misraModification = False }
    printf "\"document\",\"sentence gap activity (baseline)\",\"sentence gap activity (unmodified lda)\"\n"
    let csv_lines = [
            printf "\"%s\",\"%s\",\"%s\"\n"
                (name doc)
                (show $ DP.activity $ DP.baseline config (document doc))
                (show $ DP.activity $ DP.lda config lda (document doc))
            | doc <- testSet]
            `using` parBuffer 2 rdeepseq
    mapM_ putStr csv_lines

