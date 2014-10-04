-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.List
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.TypeTree.List (
    -- * Type constructor synonyms
    EndOfList,
    (:::),
    -- * Type-level functions
    Repeat,
    Countdown,
    Append
) where

import Data.Peano

type EndOfList = '[]

-- | Yet another type-level cons
infixr 5 :::
type (:::) = '(:)

type family Repeat (n :: Peano) v where
    Repeat Zero v = EndOfList
    Repeat (Succ n) v = v ::: Repeat n v

-- | A list of all Peano numbers, counting downwards from n. Counting up is
--   rather harder. Type-level 'Reverse' or 'Countup' functions all seem to
--   require a helper function with an accumulating parameter, which requires
--   'UndecidableInstances', e.g. for the recursive step in the below example:
--
--   > type Reverse x = ReverseHelper x EndOfList
--   >
--   > type family ReverseHelper fore back where
--   >     ReverseHelper EndOfList back  = back
--   >     ReverseHelper (x ::: xs) back = ReverseHelper xs (x ::: back)
--
--   Or similarly this, where there's an actual possibility of non-termination,
--   as opposed to the compiler simply being unable to prove termination.
--
--   > type family CountupHelper n max where
--   >     CountupHelper x x = EndOfList
--   >     CountupHelper n x = n ::: CountupHelper (Succ n) x
--
--   Note that the list is zero-indexed, i.e. the last element of the list
--   before 'EndOfList' is 'Zero' not 'Succ Zero'.
type family Countdown (n :: Peano) where
    Countdown Zero = EndOfList
    Countdown (Succ n) = n ::: (Countdown n)

-- | Appends two lists together by replacing the 'EndOfList' element on the
--   'a' with the whole of 'b'.
type family Append a b where
    Append EndOfList b = b
    Append (a ::: as) b = a ::: (Append as b)
