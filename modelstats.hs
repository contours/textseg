import qualified Data.ByteString.Char8 as BS 
--import           Data.ByteString.Char8 (ByteString)
import Control.Monad
import Text.Printf
import Python.Interpreter (py_initialize)
import System.Directory (doesFileExist)
import Data.Binary
import System.Environment (getArgs)
import System.Path.Glob
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord
import Data.Tuple
import System.Environment (getArgs)

import NLP.Tokenizer
import NLP.Segmentation
import NLP.Segmentation.TextTiling
import NLP.Segmentation.TopicTiling
import NLP.Segmentation.NLTK
import qualified NLP.Segmentation.DP as DP
import NLP.SegEval
import qualified NLP.Data
import           NLP.Data (Annotated(..),Dataset)
import Util
import NLP.LDA

main = do
    [filename] <- getArgs
    lda <- decodeFile filename
    let wm = parseWordMap lda
    let rwm = invertMap wm
    let topic_weights = map sum (transpose (p_topic_document lda))
    let word_weights = p_word_topic lda
    let top_topics = topIndices (length topic_weights) topic_weights
    printf "Top %d topic IDs: %s\n" (length top_topics) (show top_topics)
    forM_ top_topics $ \topic_id -> do
        --let top_word_ids = topIndices 20 (word_weights !! topic_id)
        let top_word_ids = error "FIXME: rewrite this line to work with the new array code"
        let top_words = map (rwm Map.!) top_word_ids
        printf "Top %d words for topic %d:\n" (length top_words) topic_id
        forM_ top_words (printf "\t%s\n" . BS.unpack)
    return ()

topIndices :: Ord a => Int -> [a] -> [Int]
topIndices n = map fst . take n . sortBy (flip (comparing snd)) . zip [0..]

invertMap :: Ord v => Map k v -> Map v k
invertMap = Map.fromList . map swap . Map.toList

