-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Map
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | 'map'-like functions at the type level
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Data.TypeTree.Map where

import Data.TypeTree.List
import Data.TypeTree.Tree

-- | Applies all type constructors in a list 'ss' to 'a'. E.g.
--
--   >>> :kind! MapCons ([] ::: Maybe ::: EndOfList) Int
--   MapCons ([] ::: Maybe ::: EndOfList) Int :: *
--       = [Int] ::: (Maybe Int ::: EndOfList)
--
--   Equivalent to the value-level expression @'map' ($ a) ss@. An open family,
--   rather than a closed one, in case you want to add even more types to it.
type family MapCons a (ss :: k1) :: k2

-- | Base case for lists
type instance MapCons a EndOfList = EndOfList

-- | Recursive case for lists
type instance MapCons a (f ::: ss) = (f a) ::: (MapCons a ss)

-- | Base case for 'Tree's
type instance MapCons a Leaf = Leaf

-- | Recursive case for 'Tree's
type instance MapCons a (Node f l r) = Node (f a) (MapCons a l) (MapCons a r)


-- | Equivalent to the value-level expression @'map' f ss@: given a list of
--   types, applies the type constructor f to each of them. An open family,
--   rather than a closed one, in case you want to add even more types to it.
type family MapArgs (f :: * -> k1) (ss :: k2) :: k3

-- | Base case for lists
type instance MapArgs f EndOfList = EndOfList

-- | Recursive case for lists
type instance MapArgs f (a ::: ss) = (f a) ::: (MapArgs f ss)

-- | Base case for 'Tree's
type instance MapArgs f Leaf = Leaf

-- | Recursive case for 'Tree's
type instance MapArgs f (Node a l r) = Node (f a) (MapArgs f l) (MapArgs f r)


-- | 'MapCons', restricted to lists. Just in case you get sick of writing
--   kind signatures everywhere.
type family MapConsList (a :: *) (ss :: [* -> k]) :: [k] where
    MapConsList a EndOfList  = EndOfList
    MapConsList a (f ::: ss) = (f a) ::: (MapConsList a ss)

-- | 'MapArgs', restricted to lists. Just in case you get sick of writing
--   kind signatures everywhere.
type family MapArgsList (f :: * -> k) (ss :: [*]) :: [k] where
    MapArgsList f EndOfList  = EndOfList
    MapArgsList f (a ::: ss) = (f a) ::: (MapArgsList f ss)

-- | 'MapCons', restricted to 'Tree's. Just in case you get sick of writing
--   kind signatures everywhere.
type family MapConsTree (a :: *) (ss :: Tree (* -> k)) :: Tree k where
    MapConsTree a Leaf         = Leaf
    MapConsTree a (Node f l r) = Node (f a) (MapConsTree a l) (MapConsTree a r)

-- | 'MapArgs', restricted to 'Tree's. Just in case you get sick of writing
--   kind signatures everywhere.
type family MapArgsTree (f :: * -> k) (ss :: Tree *) :: Tree k where
    MapArgsTree f Leaf         = Leaf
    MapArgsTree f (Node a l r) = Node (f a) (MapArgsTree f l) (MapArgsTree f r)
