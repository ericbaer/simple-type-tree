-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.THTest
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Most of this package consists of type-level hackery; pretty much the
--   only value-level stuff to test is the Template Haskell code.
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.TypeTree.THTest where

import Prelude hiding (either)

import Control.Monad
import Data.Constraint
import Data.Traversable
import Language.Haskell.TH
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.TypeTree.Operations
import Data.TypeTree.Tree
import Data.TypeTree.TH
import Data.TypeTree.THTest.Internal

-----------------------------------------------------------------------------
-- Testing of actual template haskell code
-----------------------------------------------------------------------------

$(typeInstanceCompare types)
type MyTree = $(treeType types)

-- | If it compiles, and its invocation compiles, that means the instance
--   actually exists. This is useful if some Template Haskell splice is
--   supposed to generate an instance, and you want to "test" if it really did.
checkInstance :: a => Dict a -> Assertion
checkInstance Dict = () @=? ()

justCheckThatItCompiles :: Test
justCheckThatItCompiles = testGroup "Compare" [
    testCase "MyTree has BalancedTree instance" $
        checkInstance (Dict :: Dict (BalancedTree MyTree)),
    testCase "Compare Int Int ~ EQ" $
        checkInstance (Dict :: Dict (Compare Int Int ~ EQ)),
    testCase "Compare Int Float ~ GT" $
        checkInstance (Dict :: Dict (Compare Int Float ~ GT)),
    testCase "Compare Float Int ~ LT" $
        checkInstance (Dict :: Dict (Compare Float Int ~ LT))]

-----------------------------------------------------------------------------
-- Testing of utilities
-----------------------------------------------------------------------------

-- | Recall that an 'Assertion' is actually just a type synonym for 'IO ()'.
--   Technically, we are using 'IO' as a poor version of the Template Haskell
--   'Q' monad here, which should be fine as long as we don't try to 'reify'
--   anything.
testFlattenAppT2 :: Assertion
testFlattenAppT2 = do
    functionTy <- runQ functionQ
    let result = [ConT int, ConT char, ConT double, ConT proxy `AppT` ConT int]
    result @=? flattenAppT2 ArrowT functionTy

testFlattenUnflattenIdentity :: Assertion
testFlattenUnflattenIdentity = do
    functionTy <- runQ functionQ
    let flattened = flattenAppT2 ArrowT functionTy
    functionTy @=? unflattenAppT2 ArrowT (last flattened) (init flattened)

testUnfoldAppT :: Assertion
testUnfoldAppT = do
    f3Ty <- runQ f3Q
    let result = [ConT f3, ConT int, ConT char, ConT float]
    result @=? unfoldAppT f3Ty

testFlattenAppTCon :: Assertion
testFlattenAppTCon = do
    f3ChainTy <- runQ f3ChainQ
    let result = [[ConT int, ConT char], [ConT char, ConT int],
            [ConT double, ConT float, TupleT 0]]
    result @=? flattenAppTCon (ConT f3) f3ChainTy

testUtilities :: Test
testUtilities = testGroup "Utilities" [
        testCase "unfoldAppT" testUnfoldAppT,
        testCase "flattenAppTCon" testFlattenAppTCon,
        testCase "flattenAppT2" testFlattenAppT2,
        testCase "flatten/unflatten round trip" testFlattenUnflattenIdentity]
