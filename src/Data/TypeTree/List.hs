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
    -- * Type constructors
    ListTag,
    EndOfList,
    (:::),
    -- * Type-level functions
    Repeat,
    Countdown,
    Append,
    MapConsList,
    MapArgsList
) where

import Data.Data
import Data.Peano

-- | A data type to wrap either a ':::' or an 'EndOfList'. By convention it
--   should not be used for any other purpose.
data ListTag a deriving Typeable
deriving instance (Typeable a) => Data (ListTag a)

-- | TODO: It should not be particularly painful to swap this out for built-in
--   type-level lists, if I can figure out how to write
--   'Language.Haskell.TH.Syntax.PromotedNilT':
--
--   > type EndOfList = '[] -- Is this right???
--   > infixr 5 :::
--   > type a ::: b = a ': b
--
--   Aside from updating all the Template Haskell code to deal with
--   'Language.Haskell.TH.PromotedConsT's. The annoying thing there is that
--
--   > [t| '[] |] = PromotedT GHC.Types.[]
--
--   But
--
--   > [t| '[Int] |] = AppT (AppT PromotedConsT (ConT GHC.Types.Int))
--   >    PromotedNilT
data EndOfList deriving Typeable

-- | Yet another type-level cons
infixr 5 :::
data (:::) a (b :: *) deriving Typeable

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

-- | Applies all type constructors in a list 'ss' to 'a'. E.g.
--
--   >>> :kind! MapConsList ([] ::: Maybe ::: EndOfList) Int
--   MapConsList ([] ::: Maybe ::: EndOfList) Int :: *
--       = [Int] ::: (Maybe Int ::: EndOfList)
--
--   Equivalent to the value-level expression 'map ($ a) ss'.
type family MapConsList (a :: *) (ss :: *) :: * where
    MapConsList a EndOfList  = EndOfList
    MapConsList a (f ::: ss) = (f a) ::: (MapConsList a ss)

-- | Equivalent to the value-level expression 'map f ss': given a list of
--   types, applies the type constructor f to each of them.
type family MapArgsList (f :: * -> k) (ss :: *) :: * where
    MapArgsList f EndOfList  = EndOfList
    MapArgsList f (a ::: ss) = (f a) ::: (MapArgsList f ss)
