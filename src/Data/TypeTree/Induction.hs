-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Induction
-- Copyright   :
-- License     :  BSD
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TypeTree.Induction (
    InductionWitness(..)
) where

import Data.Constraint
import Data.TypeTree.Tree
import Data.TypeTree.List

-- | A witness that a constraint 'c' holds on the tail of 'ss'. Basically,
--   pattern match on the instance constructor and the 'Dict' to let GHC
--   recover the relevant dictionary for 'c'.
data family InductionWitness (c :: k -> Constraint) (ss :: k)

-- | Base case for lists
data instance InductionWitness c EndOfList = EndOfListInductionWitness

-- | Inductive case for lists
data instance InductionWitness c (a ::: ss) = ListInductionWitness {
        _tailInstanceWitness :: Dict (c ss),
        _tailInductionWitness :: InductionWitness c ss
    }

-- | Base case for trees
data instance InductionWitness c Leaf = LeafInductionWitness

-- | Inductive case for trees
data instance InductionWitness c (Node a l r) = TreeInductionWitness {
        _leftInstanceWitness :: Dict (c l),
        _leftInductionWitness :: InductionWitness c l,
        _rightInstanceWitness :: Dict (c r),
        _rightInductionWitness :: InductionWitness c r
    }

