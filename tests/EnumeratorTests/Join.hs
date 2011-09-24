{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Join
	( test_JoinE
	) where

import           Data.Functor.Identity (Identity, runIdentity)

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_JoinE :: Suite
test_JoinE = property "joinE" prop where
	prop :: [Integer] -> Bool
	prop xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = map (* 10) xs
		
		iter = (E.joinE (E.enumList 1 xs) (EL.map (* 10))) $$ EL.consume
