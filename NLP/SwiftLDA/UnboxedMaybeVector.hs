{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             TypeFamilies, ScopedTypeVariables, BangPatterns,  
             TupleSections  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.SwiftLDA.UnboxedMaybeVector ()
where
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed.Base (MVector, Vector, Unbox)

import Control.Monad ( liftM )

newtype instance MVector s (Maybe a) = MV_Maybe (MVector s (a,Bool))
newtype instance Vector    (Maybe a) = V_Maybe  (Vector    (a,Bool))

instance (Num a, Unbox a) => Unbox (Maybe a)

nothing :: (Num a, Unbox a) => a
nothing = 0
{-# INLINE nothing #-}

instance (Num a, Unbox a) => M.MVector MVector (Maybe a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_Maybe v) = M.basicLength v
  basicUnsafeSlice i n (MV_Maybe v) = MV_Maybe $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Maybe v1) (MV_Maybe v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Maybe `liftM` M.basicUnsafeNew n
  basicUnsafeRead (MV_Maybe v) i = fromPair `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Maybe v) i mx = M.basicUnsafeWrite v i (maybe (nothing,False) (,True) mx)
  
instance (Num a, Unbox a) => G.Vector Vector (Maybe a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicLength (V_Maybe v) = G.basicLength v
  basicUnsafeFreeze (MV_Maybe v) = V_Maybe `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Maybe v) = MV_Maybe `liftM` G.basicUnsafeThaw v
  basicUnsafeSlice i n (V_Maybe v) = V_Maybe $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Maybe v) i
                = fromPair `liftM` G.basicUnsafeIndexM v i
                  
fromPair :: (Unbox a) => (a, Bool) -> Maybe a                  
fromPair (_, False) = Nothing
fromPair (x, True)  = Just x
{-# INLINE fromPair #-}