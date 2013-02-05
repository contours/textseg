{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module NLP.Segmentation
    ( LinearMass(..)
    , CharacterMass(..)
    , WordMass(..)
    , SentenceMass(..)
    , ParagraphMass(..)
    , totalWordMass
    , totalSentenceMass
    , totalParagraphMass
    , paragraphWordMass
    , paragraphSentenceMass
    , sentenceWordMass
    , roundMasses
    , indicesToMasses
    , massesToIndices
    , roundIndices
    -- "low-level interface"
    , downcastSegmentation
    , upcastSegmentation
    ) where

import qualified Data.ByteString.Char8 as BS

import NLP.Tokenizer

newtype CharacterMass = CharacterMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)
newtype WordMass = WordMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)
newtype SentenceMass = SentenceMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)
newtype ParagraphMass = ParagraphMass Int
    deriving (Eq,Show,Ord,Enum,Real,Num,Integral)

-- | This class provides operations for converting between different levels of linear segmentation, given the tokens of the original document. Downcasts (e.g. sentence masses to word masses) are exact. Upcasts (e.g. word masses to sentence masses) will round off segment boundaries to the nearest possible location.
class LinearMass s where
    toCharacterMass :: [Token] -> [s] -> [CharacterMass]
    toWordMass :: [Token] -> [s] -> [WordMass]
    toSentenceMass :: [Token] -> [s] -> [SentenceMass]
    toParagraphMass :: [Token] -> [s] -> [ParagraphMass]

    -- | General casting between linear segmentation types.
    fromLinearMass :: LinearMass t => [Token] -> [t] -> [s]

instance LinearMass CharacterMass where
    toCharacterMass _ = id
    toWordMass = error "CharacterMass toWordMass not implemented"
    toSentenceMass toks cms = map fromIntegral $ upcastSegmentation cms (sentenceCharacterMass toks)
    toParagraphMass = error "CharacterMass toParagraphMass not implemented"

    fromLinearMass = toCharacterMass

instance LinearMass WordMass where
    toCharacterMass = error "WordMass toCharacterMass not implemented"
    toWordMass _ = id
    toSentenceMass toks wms = map fromIntegral $ upcastSegmentation wms (sentenceWordMass toks)
    toParagraphMass toks wms = map fromIntegral $ upcastSegmentation wms (paragraphWordMass toks)

    fromLinearMass = toWordMass

instance LinearMass SentenceMass where
    toCharacterMass = error "SentenceMass toCharacterMass not implemented"
    toWordMass = error "SentenceMass toWordMass not implemented"
    toSentenceMass _ = id
    toParagraphMass toks sms = map fromIntegral $ upcastSegmentation sms (paragraphSentenceMass toks)

    fromLinearMass = toSentenceMass

instance LinearMass ParagraphMass where
    toCharacterMass = error "ParagraphMass toCharacterMass not implemented"
    toWordMass toks pms = map fromIntegral $ downcastSegmentation (paragraphWordMass toks) pms
    toSentenceMass toks pms = map fromIntegral $ downcastSegmentation (paragraphSentenceMass toks) pms
    toParagraphMass _ = id

    fromLinearMass = toParagraphMass

-- TODO: write QuickCheck properties for all of these conversions!

-- TODO: JSON import/export of segmentations

totalWordMass :: [Token] -> WordMass
totalWordMass = WordMass . length . filter isWord

totalSentenceMass :: [Token] -> SentenceMass
totalSentenceMass = sum . paragraphSentenceMass

totalParagraphMass :: [Token] -> ParagraphMass
totalParagraphMass = ParagraphMass . (+1) . length . filter isParagraphBreak

-- | Word mass of each paragraph.
paragraphWordMass :: [Token] -> [WordMass]
paragraphWordMass toks = map totalWordMass (splitAtParagraphs toks)

-- | Sentence mass of each paragraph.
paragraphSentenceMass :: [Token] -> [SentenceMass]
paragraphSentenceMass toks = map (SentenceMass . (+1) . length . filter isSentenceBreak . trim) (splitAtParagraphs toks)
    where trim (SentenceBreak _ : ts) = trim ts
          trim ts | isSentenceBreak (last ts) = trim (init ts)
          trim ts | otherwise = ts

-- | Word mass of each sentence.
sentenceWordMass :: [Token] -> [WordMass]
sentenceWordMass toks = map (WordMass . length . filter isWord) (splitAtSentences toks)

-- | Character mass of each sentence (including the sentence break).
sentenceCharacterMass :: [Token] -> [CharacterMass]
sentenceCharacterMass toks = map (CharacterMass . sum . map (BS.length . tokenText)) (splitAtSentences' toks)

downcastSegmentation :: (Integral a,Integral b) => [a] -> [b] -> [a]
downcastSegmentation ls1 us1 = go ls1 us1
    where go ls (u:us) = sum (take (fromIntegral u) ls) : go (drop (fromIntegral u) ls) us
          go [] _ = []
          go _ [] = []

upcastSegmentation :: Integral a => [a] -> [a] -> [a]
upcastSegmentation ls1 us1 = go 0 (roundMasses ls1 us1) us1
    where go n (0:ls) us = go n ls us
          go n (l:ls) (u:us) =
              case compare (l-u) 0 of
                   EQ -> n+1 : go 0 ls us
                   GT -> go (n+1) (l-u:ls) us
                   LT -> error "upcastSegmentation: there is a bug in roundMasses"
          go 0 [] [] = []
          go _ _ _ = error "upcastSegmentation: TODO: figure out what to do in these cases, if they ever occur"

-- | @roundMasses ls us@: as 'roundIndices', but operates on segment masses. The segment boundaries of @ls@ are shifted to coincide with the closest segment boundaries of @us@.
roundMasses :: (Num a, Ord a) => [a] -> [a] -> [a]
roundMasses ls us = indicesToMasses (roundIndices (massesToIndices ls) (massesToIndices us)) (sum us)

-- | Convert list of 0-based indices of segment beginnings to list of segment masses.
-- Requires the total mass as a parameter, to calculate the mass of the last segment.
indicesToMasses :: Num a => [a] -> a -> [a]
indicesToMasses is total = zipWith (-) (is++[total]) (0:is++[total])

-- | Convert list of segment masses to list of 0-based indices of segment beginnings.
-- Note that the length of the last segment is unspecified in this representation.
massesToIndices :: Num a => [a] -> [a]
massesToIndices = tail . scanl (+) 0 . init

-- | @roundIndices ls us@: round off each index in @ls@ to the closest one in @us@. Both lists must be in ascending order. Indices in @ls@ which are exactly halfway between two indices in @us@ get rounded to the left.
roundIndices :: (Num a, Ord a) => [a] -> [a] -> [a]
roundIndices [] _ = []
roundIndices (_:ls) [u] = u : roundIndices ls [u] -- past the end
roundIndices (l:ls) (u1:u2:us) =
    if | l == u1 -> u1 : roundIndices ls (u1:u2:us)
       | l == u2 -> u2 : roundIndices ls (u1:u2:us)
       | l >  u2 -> roundIndices (l:ls) (u2:us)
       | l <  u1 -> u1 : roundIndices ls (u1:u2:us)
       | l <  u2 -> case compare (l-u1) (u2-l) of
                         LT -> u1 : roundIndices ls (u1:u2:us)
                         EQ -> u1 : roundIndices ls (u1:u2:us)
                         GT -> u2 : roundIndices ls (u1:u2:us)
roundIndices _ [] = error "roundIndices: empty index list"

