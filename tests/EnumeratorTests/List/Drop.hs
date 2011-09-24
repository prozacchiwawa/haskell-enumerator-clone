{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Drop
	( test_Drop
	, test_Filter
	, test_FilterM
	) where

import           Data.List (genericDrop)

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_Drop :: Suite
test_Drop = property "drop" $ prop_ListN
	(\n -> EL.drop n >> EL.consume)
	(\n -> Right . genericDrop n)

test_Filter :: Suite
test_Filter = test_Enumeratee "filter" (EL.filter (\_ -> True))

test_FilterM :: Suite
test_FilterM = test_Enumeratee "filterM" (EL.filterM (\_ -> return True))
