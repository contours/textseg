import Data.Binary

import qualified NLP.Segmentation.TopicTiling as TT

import Datasets (load_training_set)

main = do
    docs <- load_training_set
    let lda = TT.train TT.defaultTrainConfig docs
    encodeFile "/srv/data/hybrid.model" lda


