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
import           EnumeratorTests.Binary.Isolate
import           EnumeratorTests.Binary.Map
import           EnumeratorTests.Binary.Require
import           EnumeratorTests.Binary.Split
import           EnumeratorTests.Binary.Zip

test_Binary :: Suite
test_Binary = suite "binary"
	[ test_Consume
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
