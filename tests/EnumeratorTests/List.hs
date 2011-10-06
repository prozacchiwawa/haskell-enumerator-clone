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
import           EnumeratorTests.List.Iterate
import           EnumeratorTests.List.Map
import           EnumeratorTests.List.Repeat
import           EnumeratorTests.List.Replicate
import           EnumeratorTests.List.Require
import           EnumeratorTests.List.Split
import           EnumeratorTests.List.Unfold
import           EnumeratorTests.List.Unique
import           EnumeratorTests.List.Zip

test_List :: Suite
test_List = suite "list"
	[ test_Consume
	, test_ConcatMap
	, test_ConcatMapM
	, test_Drop
	, test_DropWhile
	, test_Fold
	, test_FoldM
	, test_Filter
	, test_FilterM
	, test_GenerateM
	, test_Head
	, test_Head_
	, test_Isolate
	, test_Iterate
	, test_IterateM
	, test_Map
	, test_MapM
	, test_MapM_
	, test_MapAccum
	, test_MapAccumM
	, test_Repeat
	, test_RepeatM
	, test_Replicate
	, test_ReplicateM
	, test_Require
	, test_SplitWhen
	, test_Take
	, test_Unfold
	, test_UnfoldM
	, test_Unique
	, test_Zip
	]
