{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List
	( test_List
	) where

import           Test.Chell

import           EnumeratorTests.List.Consume
import           EnumeratorTests.List.Drop
import           EnumeratorTests.List.Fold
import           EnumeratorTests.List.Isolate
import           EnumeratorTests.List.Map
import           EnumeratorTests.List.Require
import           EnumeratorTests.List.Split
import           EnumeratorTests.List.Zip

test_List :: Suite
test_List = suite "list"
	[ test_Consume
	, test_ConcatMap
	, test_ConcatMapM
	, test_Drop
	, test_Fold
	, test_FoldM
	, test_Filter
	, test_FilterM
	, test_Head
	, test_Isolate
	, test_Map
	, test_MapM
	, test_MapAccum
	, test_MapAccumM
	, test_Require
	, test_SplitWhen
	, test_Take
	, test_Zip
	]
