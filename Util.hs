module Util where

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / fromIntegral (length xs)

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

