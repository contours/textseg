{- |

Skew heaps, based on the description of the algorithm at Wikipedia.

Greg Maslov <gmaslov@bootis.org>

-}
module SkewHeap (
    SkewHeap(),
    empty,
    singleton,
    null,
    insert,
    minKeyValue,
    minKey,
    minValue,
    deleteMin,
    merge
) where

import Prelude hiding (null)

-- | This is a binary tree. Each node has a key, a value, and up to two children.
data SkewHeap k a = Nil | Node k a (SkewHeap k a) (SkewHeap k a) deriving (Show)

empty :: SkewHeap k a
empty = Nil

singleton :: k -> a -> SkewHeap k a
singleton k a = Node k a Nil Nil

null :: SkewHeap k a -> Bool
null Nil = True
null _ = False

-- | Insert a node. This is accomplished by merging the existing tree with a tree containing only one node.
insert :: Ord k => k -> a -> SkewHeap k a -> SkewHeap k a
insert k a = merge (singleton k a)

-- | The minimum key of the tree and the value associated with it.
minKeyValue :: SkewHeap k a -> (k, a)
minKeyValue (Node k a _ _) = (k,a)
minKeyValue _ = error "SkewHeap.minKeyValue: empty"

-- | The minimum key of the tree.
minKey :: SkewHeap k a -> k
minKey (Node k _ _ _) = k
minKey _ = error "SkewHeap.minKey: empty"

-- | The value associated with the minimum key of the tree.
minValue :: SkewHeap k a -> a
minValue (Node _ a _ _) = a
minValue _ = error "SkewHeap.minValue: empty"

-- | Deletes the node with the minimum key. This is accomplished by merging its two children.
deleteMin :: Ord k => SkewHeap k a -> SkewHeap k a
deleteMin (Node _ _ left right) = merge left right
deleteMin Nil = error "SkewHeap.deleteMin: empty"

-- | Merge two heaps. This is the primary operation of the skew heap.
merge :: Ord k => SkewHeap k a -> SkewHeap k a -> SkewHeap k a
merge Nil x = x
merge x Nil = x
merge t1@(Node k1 a1 l1 r1) t2@(Node k2 a2 l2 r2) =
    if k1 < k2
       then Node k1 a1 (merge t2 r1) l1
       else Node k2 a2 (merge t1 r2) l2

