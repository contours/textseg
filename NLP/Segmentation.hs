{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Segmentation
    ( LinearSegmentation(..)
    , CharacterMass(..)
    , WordMass(..)
    , SentenceMass(..)
    , ParagraphMass(..)
    ) where

import NLP.Tokenizer

newtype CharacterMass = CharacterMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)
newtype WordMass = WordMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)
newtype SentenceMass = SentenceMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)
newtype ParagraphMass = ParagraphMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)

class LinearSegmentation s where
    toCharacterMass :: [Token] -> s -> [CharacterMass]
    toWordMass :: [Token] -> s -> [WordMass]
    toSentenceMass :: [Token] -> s -> [SentenceMass]
    toParagraphMass :: [Token] -> s -> [ParagraphMass]

instance LinearSegmentation [CharacterMass] where
    toCharacterMass _ = id
    toWordMass = error "CharacterMass toWordMass not implemented"
    toSentenceMass = error "CharacterMass toSentenceMass not implemented"
    toParagraphMass = error "CharacterMass toParagraphMass not implemented"

instance LinearSegmentation [WordMass] where
    toCharacterMass = error "WordMass toCharacterMass not implemented"
    toWordMass _ = id
    toSentenceMass = error "WordMass toSentenceMass not implemented"
    toParagraphMass = error "WordMass toParagraphMass not implemented"

instance LinearSegmentation [SentenceMass] where
    toCharacterMass = error "SentenceMass toCharacterMass not implemented"
    toWordMass = error "SentenceMass toWordMass not implemented"
    toSentenceMass _ = id
    toParagraphMass = error "SentenceMass toParagraphMass not implemented"

instance LinearSegmentation [ParagraphMass] where
    toCharacterMass = error "ParagraphMass toCharacterMass not implemented"
    toWordMass toks pmss = go (splitAtParagraphs toks) pmss
        where go ps (ParagraphMass m:ms) = WordMass (wordCount (concat (take m ps))) : go (drop m ps) ms
              go [] _ = []
              go _ [] = []
    toSentenceMass = error "ParagraphMass toSentenceMass not implemented"
    toParagraphMass _ = id

-- TODO: JSON import/export of segmentations

