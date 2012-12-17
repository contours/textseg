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

type Word = ByteString

-- TODO: make these transparent
data Model = Model {
    others :: ByteString,
    phi :: ByteString,
    theta :: ByteString,
    tassign :: ByteString,
    twords :: ByteString,
    wordmap :: ByteString
    }

tmpdir_prefix = "/tmp/lda-" :: String
path_to_lda = "" :: String

train :: Double -> Double -> Int -> Int -> [[Word]] -> IO Model
train alpha beta num_topics num_iterations docs =
    bracket (mkdtemp tmpdir_prefix) (removeDirectoryRecursive) $ \tmpdir -> do
        let inputname = "train"
        let inputfile = tmpdir++"/"++inputname
        BS.writeFile inputfile (BS.pack (printf "%d\n" (length docs)))
        BS.appendFile inputfile $ BS.intercalate "\n" $ map (BS.intercalate " ") docs
        -- TODO: remove twords, or make optional (debugging feature)
        errno <- withCurrentDirectory tmpdir $
            system (printf "%slda -est -alpha %f -beta %f -ntopics %d -niters %d -twords 20 -savestep %d -dfile '%s' >/dev/null"
                path_to_lda alpha beta num_topics num_iterations num_iterations inputfile)
        case errno of { ExitFailure e -> fail (printf "lda failed with exit code %d" e); _ -> return () }
        -- now read back the model info
        let prefix = tmpdir++"/model-final." 
        model <- Model <$>
            BS.readFile (prefix++"others") <*>
            BS.readFile (prefix++"phi") <*>
            BS.readFile (prefix++"theta") <*>
            BS.readFile (prefix++"tassign") <*>
            BS.readFile (prefix++"twords") <*>
            BS.readFile (tmpdir++"/wordmap.txt")
        return model

infer :: Int -> Model -> [[Word]] -> IO Model
infer num_iterations model docs =
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
        BS.writeFile (prefix++"twords")  (twords model)
        BS.writeFile (tmpdir++"/wordmap.txt") (wordmap model)
        -- TODO: remove twords, or make optional (debugging feature)
        errno <- withCurrentDirectory tmpdir $
            system (printf "%slda -inf -dir '%s' -model '%s' -niters %d -twords 20 -dfile '%s' >/dev/null"
                path_to_lda tmpdir modelname num_iterations inputname)
        case errno of { ExitFailure e -> fail (printf "lda failed with exit code %d" e); _ -> return () }
        -- read inferences
        let prefix = tmpdir++"/"++inputname++"."
        inference <- Model <$>
            BS.readFile (prefix++"others") <*>
            BS.readFile (prefix++"phi") <*>
            BS.readFile (prefix++"theta") <*>
            BS.readFile (prefix++"tassign") <*>
            BS.readFile (prefix++"twords") <*>
            pure (wordmap model)
        return inference

topic_assignments :: Model -> [[(Word,Int)]]
topic_assignments model = case parseOnly assignments (tassign model) of
                               Left err -> error err
                               Right as -> as
    where assignments = assignment `sepBy` string "\n"
          assignment = pair `sepBy` space
          pair = do
              w <- word
              string ":"
              t <- topic
              return (BS.pack w,t)
          word = many1 (satisfy (/= ':'))
          topic = decimal :: Parser Int

-- | Change to the given directory, perform an action, then change back.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
    here <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory here) action

