{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module NLP.FrequencyVector where

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Vector.Generic as V
import Data.Vector.Unboxed (Vector)
import Data.List

type Dictionary k = Map k Int

mkDictionary :: Ord k => [k] -> Dictionary k
mkDictionary words = M.fromList (zip (nub words) [0..])

frequencyVector :: (Num b, Ord k, V.Vector v b) => Map k Int -> [k] -> v b
frequencyVector dict words = V.accum (+) zero freqs1
    where zero = V.replicate (M.size dict) 0
          freqs1 = map ((,1) . lookupIndex) words
          lookupIndex w = maybe (error $ "frequencyVector: encountered an element not in the dictionary") id (M.lookup w dict)

frequencyVector'
  :: (Num b, Ord k, V.Vector v k, V.Vector v (Int, b),
      V.Vector v b) =>
     Map k Int -> v k -> v b
frequencyVector' dict words = V.accumulate (+) zero freqs1
    where zero = V.replicate (M.size dict) 0
          freqs1 = V.map ((,1) . lookupIndex) words
          lookupIndex w = maybe (error $ "frequencyVector: encountered an element not in the dictionary") id (M.lookup w dict)

cosineSimilarity :: Ord k => Map k Int -> [k] -> [k] -> Double
cosineSimilarity d a' b' = dot a b / (dot a a * dot b b)
    where a = frequencyVector d a' :: Vector Double
          b = frequencyVector d b' :: Vector Double
          dot x y = V.sum (V.zipWith (*) x y)

cosineSimilarity'
    :: (Ord k, V.Vector Vector k) =>
       Map k Int -> Vector k -> Vector k -> Double
cosineSimilarity' d a' b' = dot a b / (dot a a * dot b b)
    where a = frequencyVector' d a' :: Vector Double
          b = frequencyVector' d b' :: Vector Double
          dot x y = V.sum (V.zipWith (*) x y)

