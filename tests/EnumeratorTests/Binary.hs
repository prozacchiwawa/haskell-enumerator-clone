{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary
	( test_Binary
	) where

import           Test.Chell

import           EnumeratorTests.Binary.Consume
import           EnumeratorTests.Binary.Drop
import           EnumeratorTests.Binary.Fold
import           EnumeratorTests.Binary.Handle
import           EnumeratorTests.Binary.Isolate
import           EnumeratorTests.Binary.Iterate
import           EnumeratorTests.Binary.Map
import           EnumeratorTests.Binary.Repeat
import           EnumeratorTests.Binary.Replicate
import           EnumeratorTests.Binary.Require
import           EnumeratorTests.Binary.Split
import           EnumeratorTests.Binary.Unfold
import           EnumeratorTests.Binary.Zip

test_Binary :: Suite
test_Binary = suite "binary"
	[ test_Consume
	, test_ConcatMap
	, test_ConcatMapM
	, test_ConcatMapAccum
	, test_ConcatMapAccumM
	, test_Drop
	, test_DropWhile
	, test_EnumHandle
	, test_EnumHandleRange
	, test_Filter
	, test_FilterM
	, test_Fold
	, test_FoldM
	, test_GenerateM
	, test_Head
	, test_Head_
	, test_Isolate
	, test_IsolateWhile
	, test_Iterate
	, test_IterateM
	, test_IterHandle
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
	, test_TakeWhile
	, test_Unfold
	, test_UnfoldM
	, test_Zip
	]
