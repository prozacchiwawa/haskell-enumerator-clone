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
import           EnumeratorTests.Text.Isolate
import           EnumeratorTests.Text.Map
import           EnumeratorTests.Text.Require
import           EnumeratorTests.Text.Split
import           EnumeratorTests.Text.Zip

test_Text :: Suite
test_Text = suite "text"
	[ test_TextCodecs
	, test_Consume
	, test_ConcatMap
	, test_ConcatMapM
	, test_Drop
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
