{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Test.Chell (Suite, defaultMain)

import           EnumeratorTests.Binary (test_Binary)
import           EnumeratorTests.CatchError (test_CatchError)
import           EnumeratorTests.Compatibility (test_Compatibility)
import           EnumeratorTests.Join (test_JoinE)
import           EnumeratorTests.List (test_List)
import           EnumeratorTests.Sequence (test_Sequence)
import           EnumeratorTests.Stream (test_Stream)
import           EnumeratorTests.Text (test_Text)

tests :: [Suite]
tests =
	[ test_Binary
	, test_CatchError
	, test_Compatibility
	, test_JoinE
	, test_List
	, test_Sequence
	, test_Stream
	, test_Text
	]

main :: IO ()
main = Test.Chell.defaultMain tests
