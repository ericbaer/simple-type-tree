-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Containment.Tree
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Containment tests for type trees, without requiring 'UndecidableInstances'
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TypeTree.Containment.Tree (
    -- * Paths and types in trees
    PathToType,
    TypeAtPath,
    -- * Tree containment tests
    TreeContains,
    TreeContains1,
    TreeContains2,
    TreeContains3,
    TreeContains4,
    TreeContains5,
    TreeContains6,
    TreeContains7,
    TreeContains8,
    TreeContains9,
    TreeContains10,
) where

import Data.TypeTree.List
import Data.TypeTree.Operations
import Data.TypeTree.Tree

type family TreePathHelper a ss where
    TreePathHelper a Leaf = Leaf
    TreePathHelper a (Node a l r) = Node Yes Leaf Leaf
    TreePathHelper a (Node b l r) =
        Node No (TreePathHelper a l) (TreePathHelper a r)

-- | Prune one level of branches where both leaves are 'False's. When applied
--   repeatedly, this will eventually result in a tree where all 'Node's have a
--   'False' on one branch and the path leading to the goal on the other.
--   Further applications to a totally-pruned tree have no effect.
type family TreePathPrune a where
    TreePathPrune Leaf = Leaf
    TreePathPrune (Node Yes l r) = Node Yes Leaf Leaf
    TreePathPrune (Node No Leaf Leaf) = Leaf
    TreePathPrune (Node No a b) = Node No (TreePathPrune a) (TreePathPrune b)

-- | Type synonyms to apply 'TreePathPrune' up to 10 times, which should suffice
--   for a tree of up to 2047 types. In other words, since we aren't willing to
--   enable 'UndecidableInstances', instead we unroll our recursion manually and
--   put a tiny little stack depth limit on it.
type Prune1 a = TreePathPrune a
type Prune2 a = TreePathPrune (Prune1 a)
type Prune3 a = TreePathPrune (Prune2 a)
type Prune4 a = TreePathPrune (Prune3 a)
type Prune5 a = TreePathPrune (Prune4 a)
type Prune6 a = TreePathPrune (Prune5 a)
type Prune7 a = TreePathPrune (Prune6 a)
type Prune8 a = TreePathPrune (Prune7 a)
type Prune9 a = TreePathPrune (Prune8 a)
type Prune10 a = TreePathPrune (Prune9 a)

-- | Converts a 'TreePathPrune'd tree into a '(:::)' list. If the tree is the
--   result of a failed search, it contains only the single element 'Leaf'; we
--   return 'No' to allow simple non-containment tests of the form
--   'PathToType a ss ~ No'. However, this also means that if we make 'Node'
--   and 'Leaf' into their own kind (e.g. with @DataKinds@, this type family
--   becomes poly-kinded, which causes its own problems.
type family TreePathToList a where
    TreePathToList Leaf = No
    TreePathToList (Node Yes l r)   = EndOfList
    TreePathToList (Node No l Leaf) = LeftBranch ::: TreePathToList l
    TreePathToList (Node No Leaf r) = RightBranch ::: TreePathToList r

-- | The tree equivalent of 'Data.TypeTree.Containment.List.IndexOfType'
type PathToType a ss = TreePathToList (Prune10 (TreePathHelper a ss))

-- | The tree equivalent of 'Data.TypeTree.Containment.List.TypeAtIndex'
type family TypeAtPath a ss where
    TypeAtPath EndOfList (Node value left right) = value
    TypeAtPath (LeftBranch ::: a) (Node value left right) = TypeAtPath a left
    TypeAtPath (RightBranch ::: a) (Node value left right) = TypeAtPath a right

-- | The tree equivalent of 'Data.TypeTree.Containment.List.ListContains'
type TreeContains a t = TypeUnequals (PathToType a t) No

type TreeContains1 a ss = (TreeContains a ss ~ Yes)
type TreeContains2 a b ss = (TreeContains1 a ss, TreeContains1 b ss)
type TreeContains3 a b c ss = (TreeContains2 a b ss, TreeContains1 c ss)
type TreeContains4 a b c d ss = (TreeContains3 a b c ss, TreeContains1 d ss)
type TreeContains5 a b c d e ss = (TreeContains4 a b c d ss, TreeContains1 e ss)
type TreeContains6 a b c d e f ss = (TreeContains5 a b c d e ss,
    TreeContains1 f ss)
type TreeContains7 a b c d e f g ss = (TreeContains6 a b c d e f ss,
    TreeContains1 g ss)
type TreeContains8 a b c d e f g h ss = (TreeContains7 a b c d e f g ss,
    TreeContains1 h ss)
type TreeContains9 a b c d e f g h i ss = (TreeContains8 a b c d e f g h ss,
    TreeContains1 i ss)
type TreeContains10 a b c d e f g h i j ss = (TreeContains8 a b c d e f g h ss,
    TreeContains2 i j ss)
