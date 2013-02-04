{-# LANGUAGE ScopedTypeVariables #-}
module Util where

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / fromIntegral (length xs)

variance :: (Real a, Fractional b) => [a] -> b
variance xs = mean [(x-m)*(x-m) | x <- map realToFrac xs] where m = mean xs

stdev :: (Real a, Floating b) => [a] -> b
stdev = sqrt . variance

-- | @window width step xs@ produces consecutive sublists of @xs@ with the given width and advancing by @step@ each time. When @step == width@, the windows are non-overlapping. Some elements from the end of @xs@ may not appear, depending on how divisible its length is by @window@ and @width@.
window :: Int -> Int -> [a] -> [[a]]
window _ _ [] = []
window width step xs = if length l >= width
                          then l : window width step p
                          else []
                       where (l,p) = (take width xs, drop step xs)

-- | Return all cyclic permutations of the input list. For example, @cycles [1,2,3] == [[1,2,3],[2,3,1],[3,1,2]]@.
cycles :: [a] -> [[a]]
cycles xs = take n (window n 1 (cycle xs)) where n = length xs

-- | k-fold cross validation.
-- Randomly permuting the data set is up to the caller.
crossValidate :: forall sample model score.
       Int
    -> ([sample] -> model)
    -> (model -> [sample] -> score)
    -> [sample]
    -> [score]
-- (Requires language extension: ScopedTypeVariables)
crossValidate k train test samples = map validate folds
    where n = length samples
          nk = div n k
          subsamples :: [[sample]]
          subsamples = take k (window nk nk samples)
          folds :: [([sample],[sample])] -- [(validation,training)]
          folds = map (splitAt nk . concat) (cycles subsamples)
          validate (v,t) = test (train t) v

