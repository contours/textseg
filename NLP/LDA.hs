-- | Pure Haskell implementation of LDA. Cribbed from GibbsLDA++ @ http://gibbslda.sourceforge.net
{-# LANGUAGE FlexibleContexts #-}
module NLP.LDA
    ( Model(..)
    , estimate
    , infer
    , logLikelihood
) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Strict
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Binary hiding (Word)
import Data.List
import Data.Maybe
import Data.STRef
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.HashSet as HashSet
import System.Random

import Text.Printf

type Word = ByteString

-- | LDA model. Note that all array indices are 1-based, not 0-based.
data Model = Model {
    alpha :: !Double,
    beta :: !Double,
    num_topics :: !Int,
    -- | Vocabulary size.
    num_words :: !Int,
    -- | Number of documents that was used to estimate or infer this model.
    num_documents :: !Int,
    -- | "phi", the word-topic distributions. A num_topics by num_words matrix.
    phi :: UArray (Int,Int) Double,
    -- | "theta", the topic-document distributions. A num_documents by num_topics matrix.
    theta :: UArray (Int,Int) Double,
    -- | Topic assignment for each word in each document. AKA "z".
    tassign :: [UArray Int Int],
    wordmap :: Map Word Int,
    -- count variables
    nw :: UArray (Int,Int) Int,
    nwsum :: UArray Int Int
    }

instance Binary Model where
    get = Model
        <$> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> (Map.fromList <$> get)
        <*> get
        <*> get
    put model = do
        put (alpha model)
        put (beta model)
        put (num_topics model)
        put (num_words model)
        put (num_documents model)
        put (phi model)
        put (theta model)
        put (tassign model)
        put (Map.toList (wordmap model))
        put (nw model)
        put (nwsum model)

estimate :: RandomGen g => Double -> Double -> Int -> Int -> [[Word]] -> g -> (Model, g)
estimate alpha beta num_topics num_iterations documents rng0 = runST $ do
    -- Assign an integer to each distinct word, starting at one.
    -- Run the corpus through a HashSet to quickly find the unique words.
    let wordmap = Map.fromList $ zip (HashSet.toList (HashSet.fromList (concat documents))) [1..]
    let num_words = Map.size wordmap
    let num_documents = length documents
    let vbeta = fromIntegral num_words * beta
    let kalpha = fromIntegral num_topics * alpha
    -- (docs!m!n) is the integer ID of the n'th word of the m'th document.
    let docs = listBArray (1,num_documents) [listUArray (1,length xs) (map (wordmap Map.!) xs) | xs <- documents]
    let doc_length i = case bounds (docs!i) of (a,b) -> b - a + 1

    -- nw[i,j]: number of instances of word i assigned to topic j
    nw <- newSTUArray ((1,1),(num_words,num_topics)) (0 :: Int)
    -- nd[i,j]: number of words in document i assigned to topic j
    nd <- newSTUArray ((1,1),(num_documents,num_topics)) (0 :: Int)
    -- nwsum[j]: total number of words assigned to topic j
    nwsum <- newSTUArray (1,num_topics) (0 :: Int)
    -- ndsum[i]: total number of words in document i
    -- You'd think this would be a constant, but see the 'sampling' function.
    ndsum <- newListSTUArray (1,num_documents) (map doc_length [1..num_documents])
    -- z[i,j]: topic assigned to word j of document i
    -- (Array of STUArrays)
    z <- listBArray (1,num_documents) <$> sequence [newSTUArray (1,doc_length i) (0 :: Int) | i <- [1..num_documents]]

    rng <- newSTRef rng0

    -- initialize topic assignments randomly
    forM_ [1..num_documents] $ \m -> do
        forM_ [1..doc_length m] $ \n -> do
            topic <- withRng rng (randomR (1,num_topics))
            writeArray (z!m) n topic
            modifyArray nw (docs!m!n,topic) (+1)
            modifyArray nd (m,topic) (+1)
            modifyArray nwsum topic (+1)

    let sampling m n = do
            let w = docs!m!n
            -- remove z_i from the count variables
            topic <- readArray (z!m) n
            modifyArray nw (w,topic) (subtract 1) 
            modifyArray nd (m,topic) (subtract 1)
            modifyArray nwsum topic (subtract 1)
            modifyArray ndsum m (subtract 1)

            ndsumm <- fromIntegral <$> readArray ndsum m
            -- do multinomial sampling
            ps <- forM [1..num_topics] $ \k -> do
                nwwk <- fromIntegral <$> readArray nw (w,k)
                nwsumk <- fromIntegral <$> readArray nwsum k
                ndmk <- fromIntegral <$> readArray nd (m,k)
                return $! (nwwk + beta) / (nwsumk + vbeta) * (ndmk + alpha) / (ndsumm + kalpha)
            topic <- (1+) <$> withRng rng (multinomialSample ps)

            -- add newly estimated z_i to count variables
            modifyArray nw (w,topic) (+1) 
            modifyArray nd (m,topic) (+1)
            modifyArray nwsum topic (+1)
            modifyArray ndsum m (+1)
            return topic

    forM_ [1..num_iterations] $ \iter -> do
        -- XXX: make progress printing optional, or remove entirely, or something else
        unsafeIOToST $ printf "LDA estimation @ iteration %d/%d\n" iter num_iterations
        forM_ [1..num_documents] $ \m -> do
            forM_ [1..doc_length m] $ \n -> do
                topic <- sampling m n
                writeArray (z!m) n topic

    -- compute theta
    theta <- newSTUArray ((1,1),(num_documents,num_topics)) (0 :: Double)
    forM_ [1..num_documents] $ \m -> do
        forM_ [1..num_topics] $ \k -> do
            ndmk <- fromIntegral <$> readArray nd (m,k)
            ndsumm <- fromIntegral <$> readArray ndsum m
            writeArray theta (m,k) $ (ndmk+alpha)/(ndsumm+kalpha)
                
    -- compute phi
    phi <- newSTUArray ((1,1),(num_topics,num_words)) (0 :: Double)
    forM_ [1..num_topics] $ \k -> do
        forM_ [1..num_words] $ \w -> do
            nwwk <- fromIntegral <$> readArray nw (w,k)
            nwsumk <- fromIntegral <$> readArray nwsum k
            writeArray phi (k,w) $ (nwwk+beta)/(nwsumk+vbeta)

    frozen_phi <- unsafeFreeze phi
    frozen_theta <- unsafeFreeze theta
    frozen_z <- mapM unsafeFreeze (elems z)
    frozen_nw <- unsafeFreeze nw
    frozen_nwsum <- unsafeFreeze nwsum
    rng1 <- readSTRef rng

    return $! (Model {
        alpha = alpha,
        beta = beta,
        num_topics = num_topics,
        num_words = num_words,
        num_documents = num_documents,
        phi = frozen_phi,
        theta = frozen_theta,
        tassign = frozen_z,
        wordmap = wordmap,
        nw = frozen_nw,
        nwsum = frozen_nwsum
        }, rng1)

-- | Fields of Model updated: num_documents, tassign, phi, theta.
infer :: RandomGen g => Int -> Bool -> Model -> [[Word]] -> g -> (Model, g)
infer num_iterations mode_method model documents rng0 = runST $ do
    let num_documents = length documents
    let vbeta = fromIntegral (num_words model) * (beta model)
    let kalpha = fromIntegral (num_topics model) * (alpha model)
    -- TODO: do something cleverer than excising unknown words
    let filteredDocuments = [mapMaybe (flip Map.lookup (wordmap model)) xs | xs <- documents]
    let docs = listBArray (1,num_documents) [listUArray (1,length xs) xs | xs <- filteredDocuments]
    let doc_length i = case bounds (docs!i) of (a,b) -> b - a + 1

    -- Load count variables from original training data.
    -- Note that changes to these are NOT saved in the result.
    let old_nw = nw model
    let old_nwsum = nwsum model
    --let old_nwsum = listArray (1,num_topics) [sum [old_nw!(w,k) | w <- [1..num_words model]] | k <- [1..num_topics model]]

    -- nw[i,j]: number of instances of word i assigned to topic j
    nw <- newSTUArray ((1,1),(num_words model,num_topics model)) (0 :: Int)
    -- nd[i,j]: number of words in document i assigned to topic j
    nd <- newSTUArray ((1,1),(num_documents,num_topics model)) (0 :: Int)
    -- nwsum[j]: total number of words assigned to topic j
    nwsum <- newSTUArray (1,num_topics model) (0 :: Int)
    -- ndsum[i]: total number of words in document i
    -- You'd think this would be a constant, but see the 'sampling' function.
    ndsum <- newListSTUArray (1,num_documents) (map doc_length [1..num_documents])
    -- z[i,j]: topic assigned to word j of document i
    -- (Array of STUArrays)
    z <- listBArray (1,num_documents) <$> sequence [newSTUArray (1,doc_length i) (0 :: Int) | i <- [1..num_documents]]
    -- z_count[i,j,k]: number of times topic k was assigned to word j of document i (for mode_method)
    z_count <- listBArray (1,num_documents) <$> sequence [newSTUArray ((1,1),(doc_length i,num_topics model)) (0 :: Int) | i <- [1..num_documents]]

    rng <- newSTRef rng0

    -- initialize topic assignments randomly
    forM_ [1..num_documents] $ \m -> do
        forM_ [1..doc_length m] $ \n -> do
            topic <- withRng rng (randomR (1,num_topics model))
            writeArray (z!m) n topic
            modifyArray (z_count!m) (n,topic) (+1)
            modifyArray nw (docs!m!n,topic) (+1)
            modifyArray nd (m,topic) (+1)
            modifyArray nwsum topic (+1)

    let sampling m n = do
            let w = docs!m!n
            -- remove z_i from the count variables
            topic <- readArray (z!m) n
            modifyArray nw (w,topic) (subtract 1) 
            modifyArray nd (m,topic) (subtract 1)
            modifyArray nwsum topic (subtract 1)
            modifyArray ndsum m (subtract 1)

            ndsumm <- fromIntegral <$> readArray ndsum m
            -- do multinomial sampling
            ps <- forM [1..num_topics model] $ \k -> do
                let old_nwwk = fromIntegral $ old_nw!(w,k)
                let old_nwsumk = fromIntegral $ old_nwsum!k
                nwwk <- fromIntegral <$> readArray nw (w,k)
                nwsumk <- fromIntegral <$> readArray nwsum k
                ndmk <- fromIntegral <$> readArray nd (m,k)
                return $! (old_nwwk + nwwk + beta model) / (old_nwsumk + nwsumk + vbeta)
                        * (ndmk + alpha model) / (ndsumm + kalpha)
            topic <- (1+) <$> withRng rng (multinomialSample ps)
            -- add newly estimated z_i to count variables
            modifyArray nw (w,topic) (+1) 
            modifyArray nd (m,topic) (+1)
            modifyArray nwsum topic (+1)
            modifyArray ndsum m (+1)
            return topic

    forM_ [1..num_iterations] $ \iter -> do
        forM_ [1..num_documents] $ \m -> do
            forM_ [1..doc_length m] $ \n -> do
                topic <- sampling m n
                writeArray (z!m) n topic
                modifyArray (z_count!m) (n,topic) (+1)

    -- when mode_method is enabled, set final topic assignments (z)
    -- to most frequent topic assignments (argmax z_count)
    when mode_method $ do
        forM_ [1..num_documents] $ \m -> do
            forM_ [1..doc_length m] $ \n -> do
                top <- newSTRef 0
                top_count <- newSTRef 0
                forM_ [1..num_topics model] $ \k -> do
                    c <- readSTRef top_count
                    c' <- readArray (z_count!m) (n,k)
                    when (c' > c) $ do
                        writeSTRef top k
                        writeSTRef top_count c'
                writeArray (z!m) n =<< readSTRef top

    -- compute theta
    theta <- newSTUArray ((1,1),(num_documents,num_topics model)) (0 :: Double)
    forM_ [1..num_documents] $ \m -> do
        forM_ [1..num_topics model] $ \k -> do
            ndmk <- fromIntegral <$> readArray nd (m,k)
            ndsumm <- fromIntegral <$> readArray ndsum m
            writeArray theta (m,k) $ (ndmk+alpha model)/(ndsumm+kalpha)
                
    -- compute phi
    phi <- newSTUArray ((1,1),(num_topics model,num_words model)) (0 :: Double)
    forM_ [1..num_topics model] $ \k -> do
        forM_ [1..num_words model] $ \w -> do
            nwwk <- fromIntegral <$> readArray nw (w,k)
            nwsumk <- fromIntegral <$> readArray nwsum k
            writeArray phi (k,w) $ (nwwk+beta model)/(nwsumk+vbeta)

    frozen_phi <- unsafeFreeze phi
    frozen_theta <- unsafeFreeze theta
    frozen_z <- mapM unsafeFreeze (elems z)
    rng1 <- readSTRef rng

    return $! (model {
        num_documents = num_documents,
        phi = frozen_phi,
        theta = frozen_theta,
        tassign = frozen_z
        }, rng1)

-- | Compute the log-likelihood of a new document.
logLikelihood :: Int -> Model -> [Word] -> StdGen -> (Double, StdGen)
logLikelihood num_iterations model0 doc rng0 = let
    (model,rng1) = infer num_iterations False model0 [doc] rng0
    doc' = map (\w -> Map.findWithDefault (-1) w (wordmap model)) doc
    count v = fromIntegral (length (filter (==v) doc'))
    ll = sum [count v * log (sum [(theta model!(1,t)) * (phi model!(t,v)) | t <- [1..num_topics model]]) | v <- delete (-1) (nub doc')]
    in (ll,rng1)

-- A convenient type annotation, specializing newArray to the actual implementation (ST-Unboxed) we use.
{-# INLINE newSTUArray #-}
newSTUArray :: (MArray (STUArray s) e (ST s), Ix i) => (i, i) -> e -> ST s (STUArray s i e)
newSTUArray = newArray
{-# INLINE newListSTUArray #-}
newListSTUArray :: (MArray (STUArray s) e (ST s), Ix i) => (i, i) -> [e] -> ST s (STUArray s i e)
newListSTUArray = newListArray
{-# INLINE thawSTU #-}
thawSTU :: (Ix i, IArray a e, MArray (STUArray s) e (ST s)) => a i e -> ST s (STUArray s i e)
thawSTU = thaw
{-# INLINE listUArray #-}
listUArray :: (IArray UArray e, Ix i) => (i, i) -> [e] -> UArray i e
listUArray = listArray
{-# INLINE listBArray #-}
listBArray :: (IArray Array e, Ix i) => (i, i) -> [e] -> Array i e
listBArray = listArray
{-# INLINE modifyArray #-}
modifyArray :: (Monad m, MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

-- | Takes a list of probabilities (bin weights) and returns the index of the selected bin (and the new RNG).
{-# INLINE multinomialSample #-}
multinomialSample :: RandomGen g => [Double] -> g -> (Int,g)
multinomialSample ps rng = let (u,rng') = randomR (0, sum ps) rng
                               Just i = findIndexCumulative (> u) ps
                           in (i, rng')

{-# INLINE findIndexCumulative #-}
findIndexCumulative f (x:xs) = go 0 x xs
    where go i a (b:bs) = if f a then Just i else go (i+1) (a+b) bs
          go i a [] = if f a then Just i else Nothing

{-# INLINE withRng #-}
withRng :: STRef s a -> (a -> (b, a)) -> ST s b
withRng rng f = do
    rng0 <- readSTRef rng
    let (x, rng1) = f rng0
    writeSTRef rng rng1
    return x

