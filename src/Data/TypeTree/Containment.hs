-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Containment
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TypeTree.Containment (
    module X,
    ContainmentWitness(..)
) where

import Data.Constraint

import Data.TypeTree.Containment.List as X
import Data.TypeTree.Containment.Tree as X

-- | Pattern-matching on 'TreeContainsWitness' or 'ListContainsWitness'
--   gives you proof that @s elem ss@. This is a slightly verbose name, but it's
--   probably better than some obscure symbolic type operator like @:{~:@.
--   The instances look like they overlap, but they're really distinguished
--   by the kind of @ss@.
data family ContainmentWitness s (ss :: k)

data instance ContainmentWitness s slr =
    TreeContainmentWitness (Dict (TreeContains1 s slr))

data instance ContainmentWitness s ss =
    ListContainmentWitness (Dict (ListContains1 s ss))
