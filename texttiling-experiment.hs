import Control.Monad
import Text.Printf
import Control.Parallel.Strategies
import qualified Data.IntMap as IntMap
import Data.IntMap ((!))

import NLP.Segmentation
import qualified NLP.Segmentation.TextTiling as TT
import           NLP.Data (Annotated(..))

import Datasets (load_ds)

main = do
    testSet <- load_ds

    let configs =
            [TT.defaultConfig
                { TT.w = w
                , TT.k = k
                --, TT.threshold_multiplier = m
                , TT.smoothing_rounds = n
                , TT.smoothing_radius = s }
            | w <- [14,16..26]
            , k <- [6,8..14]
            --, m <- [-1.0,-0.75..1.0]
            , n <- [1..4]
            , s <- if n > 0 then [1..4] else []]
    printf "\"document\",\"w\",\"k\",\"n\",\"s/2\",\"sentence gap scores (nearest)\"\n"
    let sentenceGapIndices toks = map fromIntegral $ massesToIndices (sentenceWordMass toks)
    let sentenceGapScores toks result =
            map (m!) $ roundIndices (sentenceGapIndices toks) (TT.gapIndices result)
            where m = IntMap.fromAscList (zip (TT.gapIndices result) (TT.gapScores result))
    let csv_lines = [
            printf "\"%s\",%d,%d,%d,%d,\"%s\"\n"
                (name doc)
                (TT.w config)
                (TT.k config)
                (TT.smoothing_rounds config)
                (TT.smoothing_radius config)
                (show $ sentenceGapScores (document doc) $ TT.eval config (document doc))
            | doc <- testSet
            , config <- configs]
            `using` parBuffer 4 rdeepseq
    mapM_ putStr csv_lines

