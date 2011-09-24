{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Sequence
	( test_Sequence
	) where

import           Data.Functor.Identity (Identity, runIdentity)

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)
import           Test.QuickCheck.Poly (A)

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_Sequence :: Suite
test_Sequence = property "sequence" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = map Just xs
		
		iter = E.enumList n xs $$ E.joinI (E.sequence EL.head $$ EL.consume)
