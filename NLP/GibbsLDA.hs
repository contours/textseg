-- | Hackish interface to an external implementation of LDA called GibbsLDA++.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module NLP.GibbsLDA
    ( estimate
    ) where

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
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Array.Unboxed

import NLP.LDA (Model(..))

type Word = ByteString

tmpdir_prefix = "/tmp/lda-" :: String
path_to_lda = "" :: String

estimate :: Double -> Double -> Int -> Int -> [[Word]] -> IO Model
estimate alpha beta num_topics num_iterations docs =
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

        phi <- parse_array <$> BS.readFile (prefix++"phi")
        theta <- parse_array <$> BS.readFile (prefix++"theta")
        (words,tassign) <- parse_tassign <$> BS.readFile (prefix++"tassign")
        wordmap0 <- parse_wordmap <$> BS.readFile (tmpdir++"/wordmap.txt")
        -- fix up wordmap indices to be 1-based, not 0-based
        let wordmap = Map.map (+1) wordmap0

        -- num_words is the second dimension of phi
        let num_words = snd (snd (bounds phi))
        assert (Map.size wordmap == num_words) (return ())
        assert (sort (Map.elems wordmap) == [1..num_words]) (return ())
        -- num_documents is the first dimension of theta
        let num_documents = fst (snd (bounds theta))
        assert (length docs == num_documents) (return ())

        -- recalculate the count variables nw and nwsum from tassign
        let nw = accumArray (+) 0 ((1,1),(num_words,num_topics))
                 [((w,t),1) | (ws,ts) <- zip words tassign
                            , (w,t) <- zip (elems ws) (elems ts)]
        let nwsum = accumArray (+) 0 (1,num_topics)
                    [(t,n) | ((_,t),n) <- assocs nw]

        return $! Model {
            alpha = alpha,
            beta = beta,
            num_topics = num_topics,
            num_words = num_words,
            num_documents = num_documents,
            phi = phi,
            theta = theta,
            tassign = tassign,
            wordmap = wordmap,
            nw = nw,
            nwsum = nwsum
            }

-- Returns the words of the training documents in addition to the topics assigned to them.
-- (words, topics)
parse_tassign :: ByteString -> ([UArray Int Int], [UArray Int Int])
parse_tassign str =
    case parseOnly assignments str of
         Left err -> error err
         Right (unzip->(ws,ts)) ->
             ( [listArray (1,length w) w | w <- ws]
             , [listArray (1,length t) t | t <- ts])
    where assignments = assignment `sepBy` string "\n"
          assignment = unzip <$> pair `sepBy` string " "
          pair = do
              w <- word
              string ":"
              t <- topic
              -- NB: convert from 0-based to 1-based indices
              return (w+1, t+1)
          word = decimal :: Parser Int
          topic = decimal :: Parser Int

parse_array :: ByteString -> UArray (Int,Int) Double
parse_array str = case parseOnly topics str of
                       Left err -> error err
                       Right ps -> listArray ((1,1),(length (dropWhileEnd null ps), length (head ps))) (concat ps)
    where topics = topic `sepBy` string " \n"
          topic = p `sepBy` string " "
          p = rational

parse_wordmap :: ByteString -> Map Word Int
parse_wordmap str = case parseOnly themap str of
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


