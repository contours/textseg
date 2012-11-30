{- |
Interface to the Python-based NLTK library's implementation of TextTiling.

Must call @Python.Interpreter.py_initialize@ before using this module.
-}
module NLP.Segmentation.NLTK
    ( nltkTextTiling
    , nltkTextTiling'
) where

import Python.Interpreter
import Python.Exceptions
import Python.Objects
--import Foreign.C.Types
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS
import Control.Exception (assert)

import NLP.Segmentation
import NLP.Tokenizer

nltkTextTiling' :: String -> [String]
nltkTextTiling' str = unsafePerformIO $ handlePy exc2ioerror $ do
    pyImport "nltk"
    pyImport "nltk.tokenize"
    pyImport "nltk.tokenize.texttiling"
    tt <- callByName "nltk.tokenize.texttiling.TextTilingTokenizer" [] []
    tokenize <- getattr tt "tokenize"
    strs <- pyObject_CallHs tokenize [str] noKwParms :: IO [String]
    -- XXX: Bug workaround. Sometimes the last few characters or words
    -- of the returned tokenization will be cut off. I presume this is due
    -- to a bug in Unicode processing when converting between Python
    -- and Haskell strings, causing character lengths not to match up.
    -- To work around it, pad the last segment up to the length of the input.
    let fix = init strs ++ [drop (sum (map length (init strs))) str]
    assert (concat fix == str) (return fix)

-- | As @nltkTextTiling'@, but word-tokenizes the output to convert it into a list of segment word masses.
nltkTextTiling :: String -> [WordMass]
nltkTextTiling str = map (WordMass . length) wordses
    where wordses = map (filter isWord . tokenize . BS.pack) (nltkTextTiling' str)

