{- |
Interface to the SegEval segmentation evaluation tool, using the MissingPy Python FFI.

Must call @Python.Interpreter.py_initialize@ before evaluating any functions in this module.
-}
module NLP.SegEval
    ( similarity
    , agreement_fleiss_kappa
    , agreement_fleiss_pi
    ) where

import Python.Interpreter
import Python.Exceptions
import Python.Objects
import Foreign.C.Types
import System.IO.Unsafe

import NLP.Segmentation

-- | Evaluate the "S" metric of segmentation similarity.
-- Ref: Chris Fournier and Diana Inkpen. 2012. Segmentation Similarity and Agreement. Proceedings of Human Language Technologies: The 2012 Annual Conference of the North American Chapter of the Association for Computational Linguistics. (HLT ‘12). Association for Computational Linguistics.
similarity :: Integral a => [a] -> [a] -> Double
similarity a b = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.similarity"
    pyImport "segeval.similarity.SegmentationSimilarity"
    a' <- toPyObject (toCInts a)
    b' <- toPyObject (toCInts b)
    dec <- callByName "segeval.similarity.SegmentationSimilarity.similarity" [a',b'] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

agreement_fleiss_kappa :: Integral a => [[a]] -> Double
agreement_fleiss_kappa masseses = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.agreement"
    pyImport "segeval.agreement.Kappa"
    dict <- toPyObject [("item1", zipWith (\masses i -> ("coder"++show i, toCInts masses)) masseses [1..])]
    dec <- callByName "segeval.agreement.Kappa.fleiss_kappa" [dict] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

agreement_fleiss_pi :: Integral a => [[a]] -> Double
agreement_fleiss_pi masseses = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.agreement"
    pyImport "segeval.agreement.Pi"
    dict <- toPyObject [("item1", zipWith (\masses i -> ("coder"++show i, toCInts masses)) masseses [1..])]
    dec <- callByName "segeval.agreement.Pi.fleiss_pi" [dict] []
    x <- callByName "float" [dec] [] >>= fromPyObject :: IO CDouble
    return (realToFrac x)

toCInts :: Integral a => [a] -> [CInt]
toCInts = map fromIntegral

