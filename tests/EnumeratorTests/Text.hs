{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text
	( test_Text
	) where

import           Test.Chell

import           EnumeratorTests.Text.Codecs
import           EnumeratorTests.Text.Consume
import           EnumeratorTests.Text.Drop
import           EnumeratorTests.Text.Fold
import           EnumeratorTests.Text.Handle
import           EnumeratorTests.Text.Isolate
import           EnumeratorTests.Text.Iterate
import           EnumeratorTests.Text.Map
import           EnumeratorTests.Text.Repeat
import           EnumeratorTests.Text.Replicate
import           EnumeratorTests.Text.Require
import           EnumeratorTests.Text.Split
import           EnumeratorTests.Text.Unfold
import           EnumeratorTests.Text.Zip

test_Text :: Suite
test_Text = suite "text"
	[ test_TextCodecs
	, test_Consume
	, test_ConcatMap
	, test_ConcatMapM
	, test_ConcatMapAccum
	, test_ConcatMapAccumM
	, test_Drop
	, test_DropWhile
	, test_EnumHandle
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
	, test_Lines
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
