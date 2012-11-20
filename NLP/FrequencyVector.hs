{-# LANGUAGE TupleSections #-}
module NLP.WordFrequencyVector where

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import Numeric.LinearAlgebra
import Data.List

type Dictionary = Map ByteString Int

mkDictionary :: [ByteString] -> Dictionary
mkDictionary words = M.fromList (zip (nub words) [0..])

wordFrequency :: Dictionary -> [ByteString] -> Vector Double
wordFrequency dict words = accum zero (+) freqs1
    where zero = constant 0 (M.size dict)
          freqs1 = map ((,1) . lookupIndex) words
          lookupIndex w = maybe (error $ "not in dictionary: " ++ BS.unpack w) id (M.lookup w dict)

cosineSimilarity :: Dictionary -> [ByteString] -> [ByteString] -> Double
cosineSimilarity d a b = cosineSimilarity' a' b'
    where a' = wordFrequency d a
          b' = wordFrequency d b

cosineSimilarity' :: Vector Double -> Vector Double -> Double
cosineSimilarity' a b = dot a b / (norm2 a * norm2 b)


