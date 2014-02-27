{-# LANGUAGE OverloadedStrings #-}
module Datasets where
import System.Path.Glob
import qualified Data.ByteString.Char8 as BS 
import Data.Char (isAlpha, isUpper)
import Data.List
import Data.Maybe
import Data.Ord

import qualified NLP.Data
import           NLP.Data (Annotated(..),NamedSegmentation(..))
import NLP.Tokenizer

-- The Contours data set.
load_ds_contours = do
    ds_contours_rybesh <- NLP.Data.contours "/mnt/work/textseg/data/u-series"

    -- NB: remove rybesh's segs, since they don't cover all documents in the set
    let ds_contours = map (\d -> d {
            segmentation = filter (\s -> segname s /= "annotators:rybesh") (segmentation d)}) ds_contours_rybesh

    return ds_contours

-- The Contours data set plus the docsouth reference segmentations (only of those documents).
load_ds_merged = do
    ds_contours <- load_ds_contours
    ds_docsouth <- NLP.Data.contours "/mnt/work/textseg/data/docsouth"

    -- add docsouth reference segmentations to all present contours documents
    let ds_merged = zipWith (\d1 d2 -> Annotated {
            name = name d1,
            document = document d1,
            segmentation = segmentation d1 ++ segmentation d2 })
            (sortBy (comparing name) (filter (\d -> name d `elem` map name ds_contours) ds_docsouth))
            (sortBy (comparing name) ds_contours)

    return ds_merged

-- The ds_merged dataset, excluding documents for which the docsouth "segmentation" consists of only one segment.
load_ds = do
    ds <- load_ds_merged
    return $ filter (\(Annotated _ _ ss) -> length (segseg (fromJust (find ((=="annotators:docsouth").segname) ss))) > 1) ds

load_training_set = do
    filenames <- glob "/mnt/work/textseg/data/U. The*/*/*"
    texts <- mapM BS.readFile filenames
    return $ map (dropSpeakerNames . map newlineToSentenceBreak . tokenize) texts
    where
    newlineToSentenceBreak (Whitespace "\n") = SentenceBreak "\n"
    newlineToSentenceBreak other = other

-- Drops a line/sentence if all words in it are uppercase and the last punctuation is a colon.
dropSpeakerNames :: [Token] -> [Token]
dropSpeakerNames = concat . filter (not . isSpeakerName) . splitAtToken' (\t -> isSentenceBreak t || t == Whitespace "\n")
    where isSpeakerName toks =
              (isJust (find isPunctuation toks) `implies` (tokenText (last (filter isPunctuation toks)) == ":"))
              && all (BS.all (\c -> isAlpha c `implies` isUpper c)) [w | Word w <- toks]

implies :: Bool -> Bool -> Bool
implies p q = if p then q else True

