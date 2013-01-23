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
import Data.Array.IArray

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
    let wm = wordmap lda
    let rwm = invertMap wm
    let topic_weights = [sum [theta lda ! (m,k) | m <- [1..num_documents lda]] | k <- [1..num_topics lda]]
    let word_weights = phi lda
    let top_topics = map (1+) $ topIndices (num_topics lda) topic_weights
    printf "Top %d topic IDs: %s\n" (length top_topics) (show top_topics)
    forM_ top_topics $ \topic_id -> do
        let top_word_ids = map (1+) $ topIndices 20 [phi lda ! (topic_id,v) | v <- [1..num_words lda]]
        let top_words = map (rwm Map.!) top_word_ids
        printf "Top %d words for topic %d:\n" (length top_words) topic_id
        forM_ top_words (printf "\t%s\n" . BS.unpack)
    return ()

topIndices :: Ord a => Int -> [a] -> [Int]
topIndices n = map fst . take n . sortBy (flip (comparing snd)) . zip [0..]

invertMap :: Ord v => Map k v -> Map v k
invertMap = Map.fromList . map swap . Map.toList

