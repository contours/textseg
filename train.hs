import Data.Binary

import NLP.Segmentation.TopicTiling

import Datasets (load_training_set)

main = do
    docs <- load_training_set
    let lda = trainLDA docs
    encodeFile "/srv/data/interviews.model" lda


