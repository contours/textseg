{-# LANGUAGE NoMonomorphismRestriction 
, BangPatterns 
, DeriveGeneric
 #-}
-- | Latent Dirichlet Allocation
--
-- Imperative implementation of a collapsed Gibbs sampler for LDA. This
-- library uses the topic modeling terminology (documents, words,
-- topics), even though it is generic. For example if used for word
-- class induction, replace documents with word types, words with
-- features and topics with word classes.
module NLP.SwiftLDA
       ( -- * Samplers
         pass
       , passOne
         -- * Datatypes
       , LDA
       , Doc
       , D
       , W
       , Z
       , Table2D
       , Table1D
         -- * Access model information
       , Finalized (..)         
         -- * Initialization and finalization
       , initial
       , finalize
       , unfinalize
         -- * Querying evolving model
       , docTopicWeights_
       , priorDocTopicWeights_
         -- * Querying finalized model
       , docTopicWeights
       , wordTopicWeights
       , docCounts
       )
where       
-- Standard libraries  
import Prelude hiding (read, exponent)
import Data.Array.ST
import Data.STRef
import Control.Applicative 
import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Word
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                 as V
import qualified Data.IntMap                 as IntMap
import qualified Data.List                   as List
import qualified Data.Foldable               as Fold
import GHC.Generics (Generic)
import Debug.Trace

-- Package modules
import NLP.SwiftLDA.UnboxedMaybeVector ()

type Array2D s = STUArray s (Int,Int) Double
type Array1D s = STUArray s Int Double

type Table2D = IntMap.IntMap Table1D
type Table1D = IntMap.IntMap Double

type D = Int
type Z = Int  
type W = Int
type Doc = (D, U.Vector (W, Maybe Z))

-- | Abstract type holding the settings and the state of the sampler
data LDA s = 
  LDA 
  { _docTopics   :: !(STRef s (Array2D s))    -- ^ Document-topic counts
  , _wordTopics  :: !(STRef s (Array2D s))    -- ^ Word-topic counts
  , _topics      :: !(Array1D s)              -- ^ Topic counts
  , _alphasum    :: !Double                   -- ^ alpha * K Dirichlet
                                             -- parameter (topic
                                             -- sparseness)
  , _beta        :: !Double                   -- ^ beta Dirichlet
                                             -- parameter (word
                                             -- sparseness)
  , _topicNum    :: !Int                      -- ^ Number of topics K
  , _wSize       :: !(STRef s Int)            -- ^ Number of unique words
  , weights     :: !(Array1D s)              -- ^ Current token weights
  , weightSum   :: !(STRef s Double)         -- ^ Sum of current token weights
  , gen         :: !(Gen (PrimState (ST s))) -- ^ Random generator
  , _exponent   :: !(Maybe Double)
  } 
  
data Finalized = 
  Finalized 
  { docTopics  :: !Table2D  -- ^ Document topic counts
  , wordTopics :: !Table2D  -- ^ Word topic counts
  , topics     :: !Table1D  -- ^ Topics counts
  , topicDocs  :: !Table2D  -- ^ Inverse document-topic counts
  , topicWords :: !Table2D  -- ^ Inverse word-topic counts
  , alphasum   :: !Double   -- ^ alpha * K Dirichlet parameter (topic
                                             -- sparseness)
  , beta       :: !Double   -- ^ beta Dirichlet parameter (word
                            -- sparseness)
  , topicNum   :: !Int      -- ^ Number of topics K
  , wSize      :: !Int      -- ^ Number of unique words
  , exponent   :: !(Maybe Double)   -- ^ Learning rate exponent 
  }  deriving (Generic)
  
-- | For each document sum the topic counts              
docCounts :: Finalized -> Table1D
docCounts = IntMap.map (sum . IntMap.elems) . docTopics
 
unfinalize :: U.Vector Word32 -> Finalized -> ST s (LDA s)
unfinalize s f = do
    dt <- thawArray2D (docTopics f)
    wt <- thawArray2D (wordTopics f)
    LDA <$>
        new dt <*>
        new wt <*>
        thawArray1D (topics f) <*>
        pure (alphasum f) <*>
        pure (beta f) <*>
        pure (topicNum f) <*>
        new (wSize f) <*>
        -- XXX: is it right to reinit weight like this?
        newArray (0,topicNum f-1) 0 <*>
        new 0 <*>

        initialize s <*>
        pure (exponent f)

-- | Create transparent immutable object holding model information
-- from opaque internal representation
finalize :: LDA s -> ST s Finalized
finalize m = do
  dt  <- read . _docTopics $ m
  wt  <- read . _wordTopics $ m
  dtf <- freezeArray2D dt
  wtf <- freezeArray2D wt
  tf  <- freezeArray1D (_topics m)
  ws  <- read . _wSize $ m
  return $! Finalized {
      docTopics  = dtf
    , wordTopics = wtf
    , topics     = tf 
    , topicDocs  = invert dtf
    , topicWords = invert wtf
    , alphasum   = _alphasum m
    , beta       = _beta m
    , topicNum   = _topicNum m 
    , wSize      = ws
    , exponent   = _exponent m
    }
  
-- | Initial document index upper bound
iDSIZE :: Int
iDSIZE = 1000

-- | Initial word index upper bound
iWSIZE :: Int
iWSIZE = 1000

-- | @initial s k a b@ initializes model with @k@ topics, @a/k@ alpha
-- hyperparameter, @b@ beta hyperparameter and random seed @s@
initial :: U.Vector Word32 -> Int -> Double -> Double -> Maybe Double 
           -> ST s (LDA s) 
initial s k a b e = do           
  dta <- newArray ((0,0),(iDSIZE, k-1)) 0
  wta <- newArray ((0,0),(iWSIZE, k-1)) 0
  LDA <$> 
    new dta <*> 
    new wta <*> 
    newArray (0,k-1) 0 <*> 
    pure a <*>
    pure b <*>
    pure k <*>
    new 0 <*>
    newArray (0,k-1) 0 <*>
    new 0 <*>
    initialize s <*>
    pure e
                   
rho :: Double -> Int -> Double
rho e t = 1 - (1 + fromIntegral t)**(-e)
{-# INLINE rho #-}

-- | @pass batch@ runs one pass of Gibbs sampling on documents in @batch@  
pass :: Int -> LDA s -> V.Vector Doc -> ST s (V.Vector Doc)
pass t m = V.mapM (passOne t m) 

-- | Run a pass on a single doc  
passOne :: Int -> LDA s -> Doc -> ST s Doc
passOne t m doc@(!d, wz) = do
  grow m doc
  zs <- U.mapM one wz
  return (d, U.zip (U.map fst wz) (U.map Just zs))
  where r = maybe 1 (flip rho t) . _exponent $ m
        one (w, mz) = do
          case mz of
            Nothing -> return ()
            Just z   -> update (negate r) m d w z
          !z <- randomZ m d w
          update r m d w z
          return z

-- | Sample a random topic for doc d and word w        
randomZ :: LDA s -> Int -> Int -> ST s Int
randomZ m !d !w = do
  wordTopicWeights_ m d w
  !s <- read (weightSum m)
  sample (weights m) s (gen m)

  
-- | @topicWeights m d w@ sets @weights m@ to the unnormalized probabilities of
-- topics for word @w@ in document @d@ given LDA model @m@.  
-- Each call overwrites current weights
wordTopicWeights_ :: LDA s -> Int -> Int -> ST s ()
wordTopicWeights_ m !d !w = do
  let k  = _topicNum m
      a  = _alphasum m / fromIntegral k
      b  = _beta m
  v  <- fromIntegral  <$> read (_wSize m)
  dt <- read (_docTopics m)
  wt <- read (_wordTopics m)
  let ws = weights m
  write (weightSum m) 0
  (l,u) <- getBounds ws
  let go !z | z > u = return ()
      go !z = do
        nzd <- readArray dt (d,z) 
        nzw <- readArray wt (w,z)
        nz  <- readArray (_topics m) z
        let !n = (nzd + a) * (nzw + b) / (nz + v * b)
        !s <- read (weightSum m)    
        write (weightSum m) (s+n)
        writeArray ws z n
        go (z+1)
  go l      
  
-- Weight calc on Finalized
-- | @docTopicWeights m doc@ returns unnormalized topic probabilities
-- for document doc given LDA model @m@
docTopicWeights :: Finalized -> Doc -> U.Vector Double
docTopicWeights m (d, ws) = 
    U.accumulate (+) (U.replicate (topicNum m) 0)
  . U.concatMap (U.indexed . wordTopicWeights m d)
  . U.map fst 
  $ ws
{-# INLINE docTopicWeights #-}
    
priorDocTopicWeights_ :: LDA s -> D -> ST s (U.Vector Double)
priorDocTopicWeights_ m d = do
  grow m (d, U.empty)
  dt <- read (_docTopics m)
  ((_,0),(_,u)) <- getBounds dt
  U.generateM (u+1) (\z -> readArray dt (d,z))
  
docTopicWeights_ :: LDA s -> Doc -> ST s (U.Vector Double)
docTopicWeights_ m doc@(d, ws) = do
  grow m doc
  (0,u) <- getBounds (weights m)
  let r = U.replicate (_topicNum m) 0
  let one w = do
        wordTopicWeights_ m d w
        U.generateM (u+1) (readArray (weights m))
  U.foldM' (\z w -> do y <- one w 
                       return $! U.zipWith (+) z y) r 
    . U.map fst 
    $ ws
-- | @topicWeights m d w@ returns the unnormalized probabilities of
-- topics for word @w@ in document @d@ given LDA model @m@.
wordTopicWeights :: Finalized -> D -> W -> U.Vector Double
wordTopicWeights m d w =
  let k = topicNum m
      a = alphasum m / fromIntegral k
      b = beta m
      dt = IntMap.findWithDefault IntMap.empty d . docTopics $ m
      wt = IntMap.findWithDefault IntMap.empty w . wordTopics $ m
      v = fromIntegral . wSize $ m
      weights = [   (count z dt + a)     -- n(z,d) + alpha 
                  * (count z wt + b)     -- n(z,w) + beta
                  * (1/(count z (topics m) + v * b)) 
                      -- n(.,w) + V * beta
                  | z <- [0..k-1] ]
  in U.fromList weights
{-# INLINE wordTopicWeights #-}

-- | Update counts in the model corresponding to given doc, word and topic
update :: Double -> LDA s -> Int -> Int -> Int -> ST s ()  
update c m d w z = do
  dt  <- read (_docTopics m)
  wt  <- read (_wordTopics m)
  wsz <- read (_wSize m) ; write (_wSize m) (max (w+1) wsz)
  nz  <- readArray (_topics m) z ; writeArray (_topics m) z (nz+c)
  nzd <- readArray dt (d,z)     ; writeArray dt (d,z) (nzd+c)
  nzw <- readArray wt (w,z)     ; writeArray wt (w,z) (nzw+c)
  
-- | @grow m doc@ grows the @docTopic m@ and @wordTopic m@ tables as necessary
-- according to content of @doc@
grow :: LDA s -> Doc -> ST s ()
grow m (d, wz) = do
  let w = if U.null wz then 0 else U.maximum  (U.map fst wz)
  dt <- read (_docTopics m)  ; (_,(d_max,_)) <- getBounds dt 
  wt <- read (_wordTopics m) ; (_,(w_max,_)) <- getBounds wt
  when (d > d_max) (do dt' <- resize dt
                       write (_docTopics m)  dt')
  when (w > w_max) (do wt' <- resize wt
                       write (_wordTopics m) wt')
  
-- | @resize table@ creates a new array with double the size of the
-- first component of the upper bound of @arr@ and copies to content
-- of @arr@ to it.
resize :: Array2D s -> ST s (Array2D s)
resize a = do
  bs@((l1,l2),(u1_old,u2)) <- getBounds a
  trace (show bs) () `seq` return ()
  let u1 = u1_old * 2
      bs' = ((l1,l2),(u1,u2))
  b <- newArray bs' 0
  let copy !i = do
        v <- readArray a i
        writeArray b i v
  mapM_ copy (range bs)         
  return b

-- | @sample ws s g@ draws a random topic according to weights @ws@
-- which should sum up to @s@
sample :: Array1D s -> Double -> Gen s -> ST s Int  
sample ws s g = do
  !r <- uniformR (0,s) g
  findEvent r ws
  
-- | @findEvent r ws@ converts weights to cumulative weights, and
-- finds the index whose cumulative weight is >= r.
findEvent :: Double -> Array1D s -> ST s Int  
findEvent !r ws = do
  (l,u) <- getBounds ws
  let go !i !_n | i > u = return (i-1) 
      go !i !n  | n > 0.0 = do v <- readArray ws i
                               go (i+1) (n-v)
                | otherwise = return (i-1)  
  go l r


read :: STRef s a -> ST s a             
read = readSTRef
{-# INLINE read #-}

write :: STRef s a -> a -> ST s ()
write = writeSTRef
{-# INLINE write #-}

new :: a -> ST s (STRef s a) 
new = newSTRef  
{-# INLINE new #-}

-- | Swap the order of keys on Table2D
invert :: Table2D -> Table2D
invert outer = 
  List.foldl' (\z (k,k',v) -> upd v z k k')  IntMap.empty 
  [ (k',k,v)
    | (k, inner) <- IntMap.toList outer
    , (k', v) <- IntMap.toList inner ]
{-# INLINE invert #-}  

-- | Increment table m by c at key (k,k')
upd :: Double -> Table2D -> Int -> Int -> Table2D
upd c m k k' = IntMap.insertWith' (flip (IntMap.unionWith (+)))
                               k 
                               (IntMap.singleton k' c)
                               m

{-# INLINE upd #-}

freezeArray2D :: Array2D s -> ST s Table2D
freezeArray2D a = do
  bs <- getBounds a
  Fold.foldlM f IntMap.empty (range bs)
  where f z ind@(!i,!i') = do 
          !v <- readArray a ind
          if v > 0 
            then return $! upd v z i i'
            else return $! z
                 
freezeArray1D :: Array1D s -> ST s Table1D
freezeArray1D a = IntMap.fromList . filter ((>0) . snd) <$> getAssocs a

thawArray1D :: Table1D -> ST s (Array1D s)
thawArray1D t = newListArray (l,h) [IntMap.findWithDefault 0 i t | i <- [l..h]]
    where l = fst (IntMap.findMin t)
          h = fst (IntMap.findMax t)

thawArray2D :: Table2D -> ST s (Array2D s)
thawArray2D t = newListArray (l,h) [
    IntMap.findWithDefault 0 j $ IntMap.findWithDefault IntMap.empty i t
    | i <- [fst l..fst h], j <- [snd l..snd h]]
    where li = fst (IntMap.findMin t)
          hi = fst (IntMap.findMax t)
          lj = minimum (map (fst . IntMap.findMin) (IntMap.elems t))
          hj = maximum (map (fst . IntMap.findMax) (IntMap.elems t))
          l = (li,lj)
          h = (hi,hj)

count :: Int -> IntMap.IntMap Double -> Double
count z t = case IntMap.findWithDefault 0 z t of
        n | n < 0 -> error "NLP.SwiftLDA.count: negative count"
        n -> n
{-# INLINE count #-}     

