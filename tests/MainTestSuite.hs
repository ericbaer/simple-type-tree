-----------------------------------------------------------------------------
--
-- Module      :  MainTestSuite
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Tests for simple-type-tree
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Test.Framework

import Data.TypeTree.THTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "simple-type-tree" [
    justCheckThatItCompiles, testUtilities]]
