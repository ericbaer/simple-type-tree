-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.THTest.Internal
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Used to get around Template Haskell stage restrictions
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Data.TypeTree.THTest.Internal where

import Prelude hiding (either)

import Data.Typeable
import Language.Haskell.TH

types :: [Name]
types = [int, char, double, float, proxy, either, unit, string, name]

data F3 a b c = F3

functionQ :: Q Type
functionQ = [t| Int -> Char -> Double -> Proxy Int |]

f3ChainQ :: Q Type
f3ChainQ = [t| F3 Int Char (F3 Char Int (F3 Double Float ())) |]

f3Q :: Q Type
f3Q = [t| F3 Int Char Float |]

int, char, double, float, proxy, either, unit, string :: Name
int = ''Int
char = ''Char
double = ''Double
float = ''Float
proxy = ''Proxy
either = ''Either
unit = ''()
string = ''String
name = ''Name
f3 = ''F3
