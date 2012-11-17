{- |
Interface to the SegEval segmentation evaluation tool, using the MissingPy Python FFI.

Must call @Python.Interpreter.py_initialize@ before using this module.
-}
module NLP.SegEval
    ( similarity
    , agreement_multiKappa
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

agreement_multiKappa :: Integral a => [[a]] -> Double
agreement_multiKappa as = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "segeval"
    pyImport "segeval.agreement"
    as' <- mapM (toPyObject . toCInts) as
    -- TODO: make dict
    error "TODO: multiKappa"

toCInts :: Integral a => [a] -> [CInt]
toCInts = map fromIntegral

