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
{-# LANGUAGE TypeFamilies #-}
module Data.TypeTree.Containment (
    module X,
    ContainmentWitness(..)
) where

import Data.Constraint

import Data.TypeTree.Containment.List as X
import Data.TypeTree.Containment.Tree as X
import Data.TypeTree.List
import Data.TypeTree.Tree

-- | Pattern-matching on 'TreeContainsWitness' or 'ListContainsWitness'
--   gives you proof that @s elem ss@. This is a slightly verbose name, but it's
--   probably better than some obscure symbolic type operator like @:{~:@.
data family ContainmentWitness s ss

data instance ContainmentWitness s (TreeTag slr) =
    TreeContainmentWitness (Dict (TreeContains1 s slr))

data instance ContainmentWitness s (ListTag ss) =
    ListContainmentWitness (Dict (ListContains1 s ss))
