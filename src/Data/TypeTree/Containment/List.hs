-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Containment.List
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TypeTree.Containment.List (
    IndexOfType,
    TypeAtIndex,
    Length,
    Homogeneous,
    BackwardsNumbered,
    ListContains,
    ListContains1,
    ListContains2,
    ListContains3,
    ListContains4,
    ListContains5,
    ListContains6,
    ListContains7,
    ListContains8,
    ListContains9,
    ListContains10
) where

import Data.Peano

import Data.TypeTree.List
import Data.TypeTree.Operations

-- | A linear search for type 'k' in list 's'. Note that this counts up, in
--   contrast to 'BackwardsNumbered' which counts down. Normally this should
--   not be a problem because we don't use this type family directly, only
--   as the inverse of 'TypeAtIndex' which is also expecting the same
--   quirk. But see 'TypesAtIndex' for an example of problems this causes.
type family IndexOfType k s where
    IndexOfType EndOfList EndOfList = Zero
    IndexOfType k (k ::: s) = Zero
    IndexOfType k (b ::: s) = Succ (IndexOfType k s)

-- | The type at a given position, counting up (i.e. 'Zero' is the list head).
type family TypeAtIndex n s where
    TypeAtIndex Zero (a ::: s)       = a
    -- Uh oh, the dreaded k+1 pattern
    TypeAtIndex (Succ n) (a ::: s)   = TypeAtIndex n s

-- | A hack for calculating the length of a list of types
type Length xs = IndexOfType EndOfList xs

-- | A type synonym for a container where all the 'v's are the same
type Homogeneous f ks v = f ks (Repeat (Length ks) v)

-- | A type synonym for a container where all the 'k's are Peano numbers.
--   Note that the list is zero-indexed, i.e. the type key for the last element
--   of the list is 'Zero' not @'Succ' 'Zero'@. See 'Countdown' for an overview
--   of why a @ForwardsTypeList@ would be harder.
type BackwardsNumbered f vs = f (Countdown (Length vs)) vs

-- | Thanks to closed type families, we can do membership tests on lists.
type family ListContains a s where
    ListContains a EndOfList = No
    ListContains a (a ::: s) = Yes
    ListContains a (b ::: s) = ListContains a s

type ListContains1 a ss = (ListContains a ss ~ Yes)
type ListContains2 a b ss = (ListContains1 a ss, ListContains1 b ss)
type ListContains3 a b c ss = (ListContains2 a b ss, ListContains1 c ss)
type ListContains4 a b c d ss = (ListContains2 a b ss, ListContains2 c d ss)
type ListContains5 a b c d e ss =
    (ListContains3 a b c ss, ListContains2 d e ss)
type ListContains6 a b c d e f ss = (ListContains3 a b c ss,
    ListContains3 d e f ss)
type ListContains7 a b c d e f g ss = (ListContains4 a b c d ss,
    ListContains3 e f g ss)
type ListContains8 a b c d e f g h ss = (ListContains4 a b c d ss,
    ListContains4 e f g h ss)
type ListContains9 a b c d e f g h i ss = (ListContains5 a b c d e ss,
    ListContains4 f g h i ss)
type ListContains10 a b c d e f g h i j ss = (ListContains5 a b c d e ss,
    ListContains5 f g h i j ss)
