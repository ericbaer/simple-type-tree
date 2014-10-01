-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Tree
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TypeTree.Tree where

import Data.Data
import Data.Peano

import Data.Peano.Extras
import Data.TypeRepLike.STypeRep
import Data.TypeRepLike.TypeRepLike

import Data.TypeTree.Operations

data LeftBranch
data RightBranch

data Node value (left :: *) (right :: *) deriving Typeable
data Leaf deriving Typeable

-- | A data type to wrap either a 'Node' or a 'Leaf'; sometimes this is useful
--   as a temporary wrapper when we want to write type families that work on
--   both lists and trees.
data TreeTag a deriving (Typeable)
deriving instance (Typeable a) => Data (TreeTag a)

-- | Given a tree of type constructors, passes the argument 'a' to each of them.
--   See also 'Data.TypeTree.List.MapConsList'.
type family MapConsTree (a :: *) (ss :: *) where
    MapConsTree a Leaf         = Leaf
    MapConsTree a (Node f l r) = Node (f a) (MapConsTree a l) (MapConsTree a r)

-- | Given a tree of types, apply to the type constructor 'f' to each of them.
--   See also 'Data.TypeTree.List.MapArgsList'.
type family MapArgsTree (f :: * -> k) (ss :: *) where
    MapArgsTree f Leaf         = Leaf
    MapArgsTree f (Node a l r) = Node (f a) (MapArgsTree f l) (MapArgsTree f r)

-- | Go right until there is no more right subtree, and yield the type at that
--   node.
type family RightmostNode ss where
    RightmostNode (Node a l Leaf) = a
    RightmostNode (Node a l r)    = RightmostNode r

-- | Go left until there is no more left subtree, and yield the type at that
--   node.
type family LeftmostNode ss where
    LeftmostNode (Node a Leaf r) = a
    LeftmostNode (Node a l r)    = LeftmostNode l

-- | The class of type trees for which, under 'Compare', the maximum element
--   in the left subtree is less than the centre element, and the minimum
--   element in the right subtree is greater than the centre element. Since
--   this definition does not admit the case of the minima or maxima being
--   equal to the centre element, membership in this class also guarantees
--   that a type tree is a true set, with no repeated elements.
--
--   Keep in mind that an 'OrderedTree' can still be imbalanced (consider the
--   pathological case of a list-like tree where every left branch is a 'Leaf'
--   and every right branch a 'Node' with the smallest element greater than the
--   parent 'Node'). To guarantee that your search through a tree does not
--   degrade to linear time, use 'BalancedTree'.
class OrderedTree ss where
    -- | Though not strictly related to the concept of an "ordered tree",
    --   in practice we use these 'STypeRep's to decide how to navigate the
    --   tree, and navigating the tree is basically impsosible unless it's
    --   in the proper order.
    rootType :: Proxy ss -> STypeRep

-- | Base case: A 'Node' whose children are both leaves is obviously ordered.
instance (Typeable a) => OrderedTree (Node a Leaf Leaf) where
    rootType _ = likeTypeRep (Proxy :: Proxy a)

-- | Inductive case 1: a tree with an empty right branch and a non-empty left
--   branch is ordered if the left branch is ordered and the element at the
--   root is greater than the maximum element in the left branch.
instance (Typeable a, Compare (RightmostNode (Node la ll lr)) a ~ LT,
    OrderedTree (Node la ll lr)) =>
    OrderedTree (Node a (Node la ll lr) Leaf) where
    rootType _ = likeTypeRep (Proxy :: Proxy a)

-- | Inductive case 2: a tree with an empty left branch and a non-empty right
--   branch is ordered if the right branch is ordered and the element at the
--   root is less than the minimum element in the right branch.
instance (Typeable a, Compare a (LeftmostNode (Node ra rl rr)) ~ LT,
    OrderedTree (Node ra rl rr)) =>
    OrderedTree (Node a Leaf (Node ra rl rr)) where
    rootType _ = likeTypeRep (Proxy :: Proxy a)

-- | Inductive case 3: a tree with both non-empty branches is ordered if both
--   branches are ordered and the element at the root is greater than the
--   maximum element in the left branch and less than the minimum element in
--   the right branch. (Note this also implies that the intersection of the
--   two sets /L/ and /R/ of elements in each branch -- an empty set).
instance (Typeable a, Compare a (LeftmostNode (Node ra rl rr)) ~ LT,
    Compare (RightmostNode (Node la ll lr)) a ~ LT,
    OrderedTree (Node la ll lr), OrderedTree (Node ra rl rr)) =>
    OrderedTree (Node a (Node la ll lr) (Node ra rl rr)) where
    rootType _ = likeTypeRep (Proxy :: Proxy a)

-- | Tree depth by counting along the leftmost edge.
type family LeftDepth ss where
    LeftDepth Leaf = Zero
    LeftDepth (Node a l r) = Succ (LeftDepth l)

-- | Tree depth by counting along the rightmost edge.
type family RightDepth ss where
    RightDepth Leaf = Zero
    RightDepth (Node a l r) = Succ (RightDepth r)

-- | The class of trees which are not horribly imbalanced. Basically this
--   guarantees that any search in an 'OrderedTree' won't degrade to linear
--   time in the number of types. This admits only trees where the left and
--   right subtrees differ in depth by one (e.g. when the tree contains exactly
--   2^n types), but this also means that, e.g., a tree with 2^m elements in one
--   subtree and 2^(m+2)-1 elements in the other tree is still admitted by the
--   inductive step below.
class BalancedTree ss where
    depth :: Proxy ss -> (Proxy (LeftDepth ss), Proxy (RightDepth ss))
    depth _ = (Proxy, Proxy)

-- | Base case: a leaf is obviously balanced.
instance BalancedTree Leaf

-- | Inductive step: a tree is balanced if its two subtrees are balanced and
--   their depths differ by either one or zero. Theoretically it'd be better to
--   use 'MaxDepth' rather than 'LeftDepth' or 'RightDepth' here, except that
--   'MaxDepth' would require nested type family applications to write.
instance (DifferenceLTEQ One (LeftDepth l) (RightDepth r) ~ Yes,
    BalancedTree l, BalancedTree r) =>
    BalancedTree (Node a l r)
