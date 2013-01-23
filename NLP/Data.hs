{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module NLP.Data
    ( Segmentation
    , NamedSegmentation(..)
    , Document
    , Annotated(..)
    , Dataset

    , stargazer_hearst_1997
    , moonstone
    , galley2003
    , galley2003_1
    , choi

    , contours
    , toJsonRep

    , stopWords
    ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS 
import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List (transpose,group)
import Text.Printf (printf)
import qualified Data.HashSet as Set
import           Data.HashSet (HashSet)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (assert)
import System.Path.Glob (glob)
import System.FilePath (combine)
import qualified Data.Map as Map
import Data.Aeson
import Control.Monad
import Data.List

import NLP.Tokenizer
import NLP.Segmentation

--import Debug.Trace

type Segmentation t = [t]
data NamedSegmentation t = NamedSegmentation {
    segname :: String,
    segseg :: Segmentation t
    } deriving Show
type Document = [Token]
type Dataset t = [Annotated t]
data Annotated t = Annotated {
    name :: String,
    document :: Document,
    segmentation :: [NamedSegmentation t]
    }

stargazer_hearst_1997 :: FilePath -> IO (Dataset ParagraphMass)
stargazer_hearst_1997 articlePath = do
    txt <- BS.readFile articlePath
    let doc = tokenize txt
    let refs = [ NamedSegmentation "1" [2,3,3,1,3,6,3]
               , NamedSegmentation "2" [2,8,2,4,2,3]
               , NamedSegmentation "3" [2,1,2,3,1,3,1,3,2,2,1]
               , NamedSegmentation "4" [2,1,4,1,1,3,1,4,3,1]
               , NamedSegmentation "5" [3,2,4,3,5,4]
               , NamedSegmentation "6" [2,3,4,2,2,5,3]
               , NamedSegmentation "7" [2,3,2,2,3,1,3,2,3]] :: [NamedSegmentation ParagraphMass]
    return [Annotated articlePath doc refs]

moonstone :: FilePath -> IO (Dataset ParagraphMass)
moonstone path = do
    let chapters = [1..5]++[7..21] :: [Int]
    let csvNames = map (printf "%s/segmentations/ch%d.csv" path) chapters
    let txtNames = map (printf "%s/text/ch%d.txt" path) chapters
    txts <- mapM BS.readFile txtNames
    csvs <- mapM BS.readFile csvNames
    let docs = map tokenize txts
    let segs = map (\(csv,name) -> case parseOnly moonstone_segmentation csv of
                                        Right seg -> zipWith NamedSegmentation [name++show i|i<-[1..]] seg
                                        Left err -> error (printf "%s: %s" name err))
                   (zip csvs csvNames)
    return (zipWith3 Annotated txtNames docs segs)

-- | Parses a .csv file from the Moonstone dataset into a list of paragraph-mass segmentations.
moonstone_segmentation :: Parser [[ParagraphMass]]
moonstone_segmentation = do
    skipWhile (/='\n') >> skipSpace -- skip entire header line
    paras <- sepBy1 (tail <$> sepBy1 decimal (char ',')) endOfLine
    return $! map countRuns (transpose paras)
        where countRuns = map (ParagraphMass . length) . group

-- | Texts are organized by segment count.
-- @galley2003 path subset@
-- Subset is either "wsj" or "tdt".
galley2003 :: FilePath -> String -> IO [Dataset CharacterMass]
galley2003 path subset = mapM (galley2003_1 path subset) [4,6,8,10,12,14,16,18,20,22]

data Galley2003_Subset = Galley2003_TDT | Galley2003_WSJ

-- | @galley2003_1 path subset count@
-- Subset is either "wsj" or "tdt".
galley2003_1 :: FilePath -> String -> Int -> IO (Dataset CharacterMass)
galley2003_1 path subset count = do
    let docsPerCount = 50 :: Int
    let txtNames = map (printf "%s/text/%s/%d/%d.ref" path subset count) [1..docsPerCount]
    let segNames = map (printf "%s/segments/%s/%d/%d.ref" path subset count) [1..docsPerCount]
    let names = txtNames
    txts <- mapM BS.readFile txtNames
    segs <- mapM BS.readFile segNames
    -- In this dataset, each sentence is a line.
    -- Remove sentence breaks unless they are followed by a newline.
    let fixSentenceBreaks (f : Whitespace w : xs) =
            case (f, BS.elem '\n' w) of
                 (Word s, True) -> Word s : SentenceBreak w : fixSentenceBreaks xs
                 (SentenceBreak s, True) -> SentenceBreak (BS.append s w) : fixSentenceBreaks xs
                 (Punctuation s, True) -> SentenceBreak (BS.append s w) : fixSentenceBreaks xs
                 (Whitespace s, True) -> SentenceBreak (BS.append s w) : fixSentenceBreaks xs
                 -- there shouldn't be any paragraph breaks, but just in case.
                 (ParagraphBreak s, True) -> ParagraphBreak (BS.append s w) : fixSentenceBreaks xs
                 -- no newline? no sentence break.
                 (SentenceBreak s, False) -> Punctuation s : Whitespace w : fixSentenceBreaks xs
                 -- otherwise leave things unaltered
                 (_, False) -> f : fixSentenceBreaks (Whitespace w : xs)
        -- other sentence breaks get removed as well
        fixSentenceBreaks (SentenceBreak s : xs) = Punctuation s : fixSentenceBreaks xs
        fixSentenceBreaks (other : xs) = other : fixSentenceBreaks xs
        fixSentenceBreaks [] = []
    -- clean up the break left over when there's a trailing newline at the end
    let removeLastBreak xs = if isSentenceBreak (last xs) then init xs else xs
    let docs = map (removeLastBreak . fixSentenceBreaks . tokenize) txts
    let parse seg txt name = case parseOnly (decimal `sepBy` skipSpace) seg of
                                  Right is ->
                                      if and ['\n' == BS.index txt (i-1) | i<-is]
                                         then indicesToMasses is (BS.length txt)
                                         else error (printf "%s: not all segment boundaries correspond to newlines" name)
                                  Left err -> error (printf "%s: %s" name err)
    let masses = map (map CharacterMass) $ zipWith3 parse segs txts segNames
    return (zipWith3 Annotated names docs [[NamedSegmentation (show i) ms] | ms <- masses | i <- [1..]])

-- | Accepts a path containing *.ref files, such as "/home/you/data/choi/2/6-8"
choi :: FilePath -> IO (Dataset SentenceMass)
choi path = do
    names <- glob (combine path "*.ref")
    txts <- mapM BS.readFile names
    let f = splitAtToken (\t -> tokenText t == "==========") . simpleTokenize
    -- 'init' drops the extra trailing sentence break from each segment
    let doc = concat . map init . f
    let seg i txt = [NamedSegmentation (show i) (map (SentenceMass . length . drop 1 . filter isSentenceBreak) (f txt))]
    return (zipWith3 Annotated names (map doc txts) (zipWith seg [1..] txts))

stopWords :: HashSet BS.ByteString
stopWords = Set.empty
{-
stopWords = Set.unions
    -- Choi's stopwords list.
    [ Set.fromList $ BS.lines $ unsafePerformIO $ BS.readFile "data/choi_stopwords.list"
    -- Some additional suffixes.
    , Set.fromList ["'d","'re","'ve","'ll"]
    ]
    -}

data JsonRep t = JsonRep {
    -- TODO: add the type. assumed linear.
    jsItems :: [JsonDoc t]
    }

data JsonDoc t = JsonDoc {
    jsName :: ByteString,
    jsSegs :: [NamedSegmentation t]
    }

instance FromJSON t => FromJSON (JsonRep t) where
    parseJSON (Object v) = do
        hm <- v .: "items"
        JsonRep <$> forM (Map.toList hm) doc
        where doc (name, item) = JsonDoc <$> pure name <*> (mapM seg =<< return . Map.toList =<< parseJSON item)
              seg (coder, item) = NamedSegmentation <$> pure coder <*> parseJSON item

instance Integral t => ToJSON (JsonRep t) where
    toJSON (JsonRep docs) = object
        [ "id" .= ("exported-from-textseg"::String)
        , "segmentation_type" .= ("linear"::String)
        , "items" .= object (map f docs) ]
        where f (JsonDoc name segs) =
                  Text.pack (BS.unpack name) .= object (map g segs)
              g (NamedSegmentation name masses) =
                  Text.pack name .= map toInteger masses

deriving instance FromJSON CharacterMass
deriving instance FromJSON WordMass
deriving instance FromJSON SentenceMass
deriving instance FromJSON ParagraphMass

toJsonRep :: Dataset t -> JsonRep t
toJsonRep docs = JsonRep (map doc docs)
    where doc (Annotated name _ segs) = JsonDoc (BS.pack name) segs

-- Assumes interview texts are strictly one-sentence-per-line, without speaker names, and with no further explicit tokenization.
contours :: FilePath -> IO (Dataset SentenceMass)
contours path = do
    repTxt <- BSL.readFile (combine path "segmentations.json")
    JsonRep jss <- either fail return (eitherDecode repTxt) :: IO (JsonRep SentenceMass)
    forM jss $ \(JsonDoc (BS.unpack->name) segs) -> do
        let basename = maybe (error (printf "interview ID must start with \"interviews:\": %s" name))
                             (id)
                             (stripPrefix "interviews:" name)
        let filename = combine path basename ++ ".txt"
        txt <- BS.readFile filename
        let doc = breakPunctuation $ breakContractions $ simpleTokenize txt
        return (Annotated basename doc [NamedSegmentation name (dropWhile (==0) s) | NamedSegmentation name s <- segs])

