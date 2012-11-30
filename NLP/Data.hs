module NLP.Data
    ( Segmentation
    , Document
    , Annotated(..)
    , Dataset

    , stargazer_hearst_1997
    , moonstone
    ) where

import qualified Data.ByteString.Char8 as BS 
--import           Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List (transpose,group)
import Text.Printf (printf)

import NLP.Tokenizer
import NLP.Segmentation

--import Debug.Trace

type Segmentation t = [t]
type Document = [Token]
type Dataset t = [Annotated t]
data Annotated t = Annotated Document [Segmentation t]

stargazer_hearst_1997 :: FilePath -> IO (Dataset ParagraphMass)
stargazer_hearst_1997 articlePath = do
    txt <- BS.readFile articlePath
    let doc = tokenize txt
    let refs = [ [2,3,3,1,3,6,3]
               , [2,8,2,4,2,3]
               , [2,1,2,3,1,3,1,3,2,2,1]
               , [2,1,4,1,1,3,1,4,3,1]
               , [3,2,4,3,5,4]
               , [2,3,4,2,2,5,3]
               , [2,3,2,2,3,1,3,2,3]] :: [[ParagraphMass]]
    return [Annotated doc refs]

moonstone :: FilePath -> IO (Dataset ParagraphMass)
moonstone path = do
    let chapters = [1..5]++[7..21] :: [Int]
    let csvNames = map (printf "%s/segmentations/ch%d.csv" path) chapters
    let txtNames = map (printf "%s/text/ch%d.txt" path) chapters
    txts <- mapM BS.readFile txtNames
    csvs <- mapM BS.readFile csvNames
    let docs = map tokenize txts
    let segs = map (\(csv,name) -> case parseOnly moonstone_segmentation csv of
                                        Right seg -> seg
                                        Left err -> error (printf "%s: %s" name err))
                   (zip csvs csvNames)
    return (zipWith Annotated docs segs)

-- | Parses a .csv file from the Moonstone dataset into a list of paragraph-mass segmentations.
moonstone_segmentation :: Parser [[ParagraphMass]]
moonstone_segmentation = do
    skipWhile (/='\n') >> skipSpace -- skip entire header line
    paras <- sepBy1 (tail <$> sepBy1 decimal (char ',')) endOfLine
    return $! map countRuns (transpose paras)
        where countRuns = map (ParagraphMass . length) . group

