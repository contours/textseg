{- |
Author: Greg Maslov <maslov@cs.unc.edu>

TODO: Write a version of each function that operates on vectors instead of lists.
-}
{-# LANGUAGE ViewPatterns #-}
module Math.Combinatorics (
    facts,
    factoradic,
    combinadic,
    combinadics,
    combinadics1,
    permutation,
    randomPermutation,
    binomial,
    numPermutations,
    permutations,
    rPermutations,
    combination,
    combinations,
    combination2,
    combinations2,
    combinationsInf
    ) where

import Data.List hiding (permutations)
import qualified Data.Sequence as S
import           Data.Sequence ((><), ViewL(..))
import Data.Maybe
import Control.Monad.Random

-- | Infinite list of factorials.
facts :: Integral a => [a]
facts = scanl (*) 1 [1..]

-- | Memoizing factorial function.
fact :: Integral a => a -> a
-- FIXME: I'm paranoid about the performance of genericIndex now.
fact n = genericIndex facts n

-- | Convert a number into factoradic representation.
factoradic :: Integral a => a -> [a]
factoradic n1 = factoradic' n1 (reverse $ takeWhile (<=n1) facts)
    where
    factoradic' 0 ps = replicate (length ps) 0
    factoradic' n (p:ps)
        | n >= p    = n `div` p : factoradic' (n `mod` p) ps
        | otherwise = 0 : factoradic' n ps
    factoradic' _ [] = error "factoradic: negative argument"

-- | @combinadic n k i@ : the @i@'th set of indices representing a combination of @k@ elements out of a set of @n@.
combinadic :: Integral a => a -> a -> a -> [a]
combinadic _ k 0 = [k-1,k-2..0]
combinadic n k x = c : combinadic n (k-1) (x-x')
    where (x',c) = fromJust $ find ((<=x) . fst) [(binomial i k, i) | i <- [n-1,n-2..k]]

-- | @combinadics n k@ : values of @combinadic n k i@ for all valid values of @i@.
combinadics :: Integral a => a -> a -> [[a]]
combinadics n k = [combinadic n k i | i <- [0..binomial n k - 1]]

-- | @combinadics1 n k@ is equivalent to the difference between @combinadics n k@ and @combinadics (n-1) k@, only more efficiently calculated.
combinadics1 :: Integral a => a -> a -> [[a]]
combinadics1 n k = [combinadic n k i | i <- [binomial (n-1) k..binomial n k - 1]]

-- | kth lexicographic permutation of xs.
permutation :: Integral b => [a] -> b -> [a]
permutation xs k1 = loop (S.fromList xs) indices
    where
    indices = map fromIntegral $ padLeft 0 (length xs) (factoradic k1)
    loop xxs [] | S.null xxs = []
    loop xxs (i:is) = let (l,S.viewl -> x :< r) = S.splitAt i xxs
                      in x : loop (l >< r) is
    loop _ [] = error "permutation: ran out of factoradics!?"

-- | Pad @xs@ up to a total length @l@ using copies of @a@
padLeft :: a -> Int -> [a] -> [a]
padLeft a l xs = replicate (l - length xs) a ++ xs

-- | If you're looking for @randomCombination@, use @take n <$> randomPermutation@.
randomPermutation :: MonadRandom m => [a] -> m [a]
randomPermutation xs = do
    let l = genericLength xs :: Integer
    k <- getRandomR (0, (numPermutations l l) - 1)
    return $ permutation xs k

-- | Binomial coefficient. Also known as "n choose k".
binomial :: Integral a => a -> a -> a
binomial n k = top `div` bottom
    where top    = product [max (n-k+1) (k+1)..n]
          bottom = product [2..min k (n-k)]

-- | @numPermutations n r@ : Total number of permutations of each size r subset of a set of size n. This is @ r! * (n choose r)@
numPermutations :: Integral a => a -> a -> a
numPermutations n r = (fact r) * binomial n r

-- | lexicographic permutations of xs
permutations :: [a] -> [[a]]
permutations xs = map (permutation xs) [0..fact (fromIntegral $ length xs) - 1 :: Integer]

-- | permutations of all length-r subsets of xs (not in a particular order)
rPermutations :: Integral b => [a] -> b -> [[a]]
rPermutations xs r = concatMap permutations $ combinations xs r

-- | ith lexicographic combination of a length-k subset of xs
combination :: Integral b => [a] -> b -> b -> [a]
combination xs k i = map (genericIndex xs) (combinadic (fromIntegral $ length xs) k i)

-- | all combinations of a length-k subset of xs, in lexicographic order
combinations :: Integral b => [a] -> b -> [[a]]
combinations xs k = map (combination xs k) [0..(binomial (fromIntegral $ length xs) k) - 1]

combination2 :: Integral b => [a] -> b -> (a,a)
combination2 xs i = let [i1,i2] = combinadic (genericLength xs) 2 i
                    in (genericIndex xs i1, genericIndex xs i2)

-- | all 2-combinations of xs
combinations2 :: [a] -> [(a,a)]
combinations2 xs = map (combination2 xs) [0..(binomial (genericLength xs) 2) - 1]

-- | A version of @combinations@ for infinite lists. Doesn't work with finite lists. FIXME: This is very inefficient, using genericIndex on the whole list for each element of each combination.
combinationsInf :: Integer -> [a] -> [[a]]
combinationsInf 1 xs = map (\x -> [x]) xs
combinationsInf k xs = concat [map (map (genericIndex xs)) (combinadics1 n k) | n <- [k-1..]]

