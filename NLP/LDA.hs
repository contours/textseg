-- | http://gibbslda.sourceforge.net
{-# LANGUAGE OverloadedStrings #-}
module NLP.LDA where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.Process
import System.Posix.Temp
import Text.Printf
import System.Exit
import Control.Exception
import System.Directory
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Binary hiding (Word)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.List

import Debug.Trace

type Word = ByteString

data Model = Model {
    others :: ByteString,
    phi :: ByteString,
    theta :: ByteString,
    tassign :: ByteString,
    --twords :: ByteString,
    wordmap :: ByteString
    }

instance Binary Model where
    get = Model <$> get <*> get <*> get <*> get <*> get
    put (Model a b c d e) = mapM_ put [a,b,c,d,e]

tmpdir_prefix = "/tmp/lda-" :: String
path_to_lda = "" :: String

train :: Double -> Double -> Int -> Int -> [[Word]] -> IO Model
train alpha beta num_topics num_iterations docs =
    bracket (mkdtemp tmpdir_prefix) (removeDirectoryRecursive) $ \tmpdir -> do
        let inputname = "train"
        let inputfile = tmpdir++"/"++inputname
        BS.writeFile inputfile (BS.pack (printf "%d\n" (length docs)))
        BS.appendFile inputfile $ BS.intercalate "\n" $ map (BS.intercalate " ") docs
        errno <- withCurrentDirectory tmpdir $
            system (printf "%slda -est -alpha %f -beta %f -ntopics %d -niters %d -savestep %d -dfile '%s' >/dev/null"
                path_to_lda alpha beta num_topics num_iterations num_iterations inputfile)
        case errno of { ExitFailure e -> fail (printf "lda failed with exit code %d" e); _ -> return () }
        -- now read back the model info
        let prefix = tmpdir++"/model-final." 
        model <- Model <$>
            BS.readFile (prefix++"others") <*>
            BS.readFile (prefix++"phi") <*>
            BS.readFile (prefix++"theta") <*>
            BS.readFile (prefix++"tassign") <*>
            --BS.readFile (prefix++"twords") <*>
            BS.readFile (tmpdir++"/wordmap.txt")
        return model

infer :: Int -> Bool -> Model -> [[Word]] -> IO Model
infer num_iterations mode_method model docs =
    bracket (mkdtemp tmpdir_prefix) (removeDirectoryRecursive) $ \tmpdir -> do
        let inputname = "infer"
        let inputfile = tmpdir++"/"++inputname
        BS.writeFile inputfile (BS.pack (printf "%d\n" (length docs)))
        BS.appendFile inputfile $ BS.intercalate "\n" $ map (BS.intercalate " ") docs
        let modelname = "model"
        -- write model
        let prefix = tmpdir++"/"++modelname++"."
        BS.writeFile (prefix++"others")  (others model)
        BS.writeFile (prefix++"phi")     (phi model)
        BS.writeFile (prefix++"theta")   (theta model)
        BS.writeFile (prefix++"tassign") (tassign model)
        --BS.writeFile (prefix++"twords")  (twords model)
        BS.writeFile (tmpdir++"/wordmap.txt") (wordmap model)
        errno <- withCurrentDirectory tmpdir $
            system (printf "%slda -inf -dir '%s' -model '%s' -niters %d -dfile '%s' -modemethod %d >/dev/null"
                path_to_lda tmpdir modelname num_iterations inputname (if mode_method then 1::Int else 0))
        case errno of { ExitFailure e -> fail (printf "lda failed with exit code %d" e); _ -> return () }
        -- read inferences
        let prefix = tmpdir++"/"++inputname++"."
        inference <- Model <$>
            BS.readFile (prefix++"others") <*>
            BS.readFile (prefix++"phi") <*>
            BS.readFile (prefix++"theta") <*>
            BS.readFile (prefix++"tassign") <*>
            --BS.readFile (prefix++"twords") <*>
            pure (wordmap model)
        return inference

topic_assignments :: Model -> [[(Word,Int)]]
topic_assignments model = case parseOnly assignments (tassign model) of
                               Left err -> error err
                               Right as -> as
    where assignments = assignment `sepBy` string "\n"
          assignment = pair `sepBy` string " "
          pair = do
              w <- word
              string ":"
              t <- topic
              return (BS.pack w,t)
          word = many1 (satisfy (/= ':'))
          topic = decimal :: Parser Int

-- | The topic-document distributions; a num_documents by num_topics matrix.
p_topic_document :: Model -> [[Double]]
p_topic_document model = case parseOnly documents (theta model) of
                              Left err -> error err
                              Right ps -> dropWhileEnd null ps
    -- NB: separator is space AND newline
    where documents = document `sepBy` string " \n"
          document = p `sepBy` string " "
          p = rational

-- | The word-topic distributions; a num_topics by num_words matrix.
p_word_topic :: Model -> [[Double]]
p_word_topic model = case parseOnly topics (phi model) of
                          Left err -> error err
                          Right ps -> dropWhileEnd null ps
    where topics = topic `sepBy` string " \n"
          topic = p `sepBy` string " "
          p = rational

-- | Compute the log-likelihood of a new document.
logLikelihood :: Int -> Model -> [Word] -> IO Double
logLikelihood num_iterations model0 doc = do
    model <- infer num_iterations False model0 [doc]
    let [theta] = p_topic_document model
        phi = p_word_topic model
        num_topics = length theta
        num_words = length (head phi)
        doc' = map (\w -> Map.findWithDefault (-1) w (parseWordMap model)) doc
        count v = fromIntegral (length (filter (==v) doc'))
        ll = sum [count v * log (sum [(theta!!t) * (phi!!t!!v) | t <- [0..num_topics-1]]) | v <- [0..num_words-1]]
    print ll
    return ll

parseWordMap :: Model -> Map Word Int
parseWordMap model = case parseOnly themap (wordmap model) of
                          Left err -> error err
                          Right wm -> Map.fromList wm
    where themap = decimal >> skipSpace >> (pair `sepBy` skipSpace)
          pair = do
              l <- takeTill isSpace
              skipSpace
              r <- decimal
              skipSpace
              return (l,r)

-- | Change to the given directory, perform an action, then change back.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
    here <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory here) action

