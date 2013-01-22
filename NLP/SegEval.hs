{- |
Interface to the SegEval segmentation evaluation tool, using the MissingPy Python FFI.

Must call @Python.Interpreter.py_initialize@ before evaluating any functions in this module.
-}
{-# LANGUAGE ParallelListComp #-}
module NLP.SegEval
    ( similarity
    , similarity'
    , agreement_fleiss_kappa
    , agreement_fleiss_pi
    , artstein_poesio_bias
    , pk
    , mean_pairwise
    , windowdiff
    , windowdiff'
    ) where

import Python.Interpreter
import Python.Exceptions
import Python.Objects
import Foreign.C.Types
import System.IO.Unsafe
import Data.Ratio
import Data.List (genericLength)

-- | Uses the default parameter of n=2.
similarity :: Integral a => [a] -> [a] -> Double
similarity = similarity' 2

-- | Evaluate the S metric of segmentation similarity.
-- Parameters: n, segmentation A, segmentation B.
-- n is "the maximum number of PBs that boundaries can span to be considered transpositions".
-- Ref: Chris Fournier and Diana Inkpen. 2012. Segmentation Similarity and Agreement. Proceedings of Human Language Technologies: The 2012 Annual Conference of the North American Chapter of the Association for Computational Linguistics. (HLT ‘12). Association for Computational Linguistics.
similarity' :: Integral a => a -> [a] -> [a] -> Double
similarity' n a b = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.similarity"
    pyImport "segeval.similarity.SegmentationSimilarity"
    a' <- toPyObject (toCInts a)
    b' <- toPyObject (toCInts b)
    n' <- toPyObject (toCInt n)
    dec <- callByName "segeval.similarity.SegmentationSimilarity.similarity" [a',b'] [("n",n')]
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

-- | Evaluate the Pk metric of segmentation similarity.
pk :: Integral a => [a] -> [a] -> Double
pk a b = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.window"
    pyImport "segeval.window.Pk"
    a' <- toPyObject (toCInts (massesToPositions a))
    b' <- toPyObject (toCInts (massesToPositions b))
    dec <- callByName "segeval.window.Pk.pk" [a',b'] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

-- | Evaluate the WindowDiff metric of segmentation dissimilarity.
-- Parameters: hypothesis, reference.
-- The window size is calculated using 'compute_window_size'.
windowdiff :: Integral a => [a] -> [a] -> Double
windowdiff hyp ref = windowdiff' (compute_window_size ref) hyp ref

-- | Evaluate the WindowDiff metric of segmentation dissimilarity.
-- Parameters: window size, hypothesis, reference
windowdiff' :: Integral a => a -> [a] -> [a] -> Double
windowdiff' k a b = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.window"
    pyImport "segeval.window.WindowDiff"
    a' <- toPyObject (toCInts (massesToPositions a))
    b' <- toPyObject (toCInts (massesToPositions b))
    k' <- toPyObject (toCInt k)
    dec <- callByName "segeval.window.WindowDiff.window_diff" [a',b'] [("window_size", k')]
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

-- | Compute default WindowDiff window size: half of the mean reference segment mass.
compute_window_size :: Integral a => [a] -> a
compute_window_size masses = round (sum masses % (2 * genericLength masses))

mean_pairwise :: Integral a => ([a] -> [a] -> Double) -> [[a]] -> Double
mean_pairwise fn segs = mean [fn (segs!!a) (segs!!b) | a <- [0..length segs-1], b <- [0..length segs-1], a /= b]
    where mean xs = sum xs / fromIntegral (length xs)

massesToPositions :: Integral a => [a] -> [Int]
massesToPositions ms = concat [replicate (fromIntegral m) i | m <- ms | i <- [1..]]

-- | Takes a list of items, each of which is a list of codings, each of which is a list of segment masses.
-- Each item must have the same number of codings, and they must be in the same (corresponding) order.
toDict items = toPyObject [
    ("item"++show i, [
        ("coder"++show j, toCInts masses)
        | masses <- item | j <- [1..]])
    | item <- items | i <- [1..]]

agreement_fleiss_kappa :: Integral a => [[[a]]] -> Double
agreement_fleiss_kappa items = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.agreement"
    pyImport "segeval.agreement.Kappa"
    dict <- toDict items
    dec <- callByName "segeval.agreement.Kappa.fleiss_kappa" [dict] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

agreement_fleiss_pi :: Integral a => [[[a]]] -> Double
agreement_fleiss_pi items = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.agreement"
    pyImport "segeval.agreement.Pi"
    dict <- toDict items
    dec <- callByName "segeval.agreement.Pi.fleiss_pi" [dict] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

artstein_poesio_bias :: Integral a => [[[a]]] -> Double
artstein_poesio_bias items = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.agreement"
    pyImport "segeval.agreement.Bias"
    dict <- toDict items
    dec <- callByName "segeval.agreement.Bias.artstein_poesio_bias" [dict] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

toCInts :: Integral a => [a] -> [CInt]
toCInts = map fromIntegral

toCInt :: Integral a => a -> CInt
toCInt = fromIntegral

