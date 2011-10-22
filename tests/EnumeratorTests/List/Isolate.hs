{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Isolate
	( test_Isolate
	) where

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator ((=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_Isolate :: Suite
test_Isolate = suite "isolate"
	[ prop_Isolate
	, test_DropExtra
	, test_HandleEOF
	, test_BadParameter
	]

prop_Isolate :: Suite
prop_Isolate = property "model" $ prop_List
	(do
		x <- EL.isolate 2 =$ EL.head
		extra <- EL.consume
		return (x, extra))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:[]) -> (Just x, [])
		(x:_:xs') -> (Just x, xs'))

test_DropExtra :: Suite
test_DropExtra = assertions "drop-extra" $ do
	$expect $ equal
		(Just 'A', ['C'])
		(E.runLists_ [[], ['A'], ['B'], ['C']] $ do
			x <- EL.isolate 2 =$ EL.head
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 'A', ['C'])
		(E.runLists_ [['A', 'B', 'C']] $ do
			x <- EL.isolate 2 =$ EL.head
			extra <- EL.consume
			return (x, extra))

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(E.runLists_ [] $ do
			x <- EL.isolate 2 =$ EL.head
			extra <- EL.consume
			return (x, extra))

test_BadParameter :: Suite
test_BadParameter = assertions "bad-parameter" $ do
	$expect $ equal
		(Nothing, ['A', 'B', 'C'])
		(E.runLists_ [['A'], ['B'], ['C']] $ do
			x <- EL.isolate 0 =$ EL.head
			extra <- EL.consume
			return (x, extra))
