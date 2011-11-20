{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Isolate
	( test_Isolate
	, test_IsolateWhile
	) where

import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util (prop_Text)

test_Isolate :: Suite
test_Isolate = suite "isolate"
	[ prop_Isolate
	, test_Isolate_DropExtra
	, test_Isolate_HandleEOF
	, test_Isolate_BadParameter
	]

prop_Isolate :: Suite
prop_Isolate = property "model" $ prop_Text
	(do
		x <- E.joinI (ET.isolate 2 $$ ET.head)
		extra <- ET.consume
		return (x, extra))
	(\text -> Right $ case TL.unpack text of
		[] -> (Nothing, TL.empty)
		(x:[]) -> (Just x, TL.empty)
		(x:_:xs') -> (Just x, TL.pack xs'))

test_Isolate_DropExtra :: Suite
test_Isolate_DropExtra = assertions "drop-extra" $ do
	$expect $ equal
		(Just 'A', ["C"])
		(E.runLists_ [[], ["A"], ["B"], ["C"]] $ do
			x <- ET.isolate 2 =$ ET.head
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 'A', ["C"])
		(E.runLists_ [["A", "B", "C"]] $ do
			x <- ET.isolate 2 =$ ET.head
			extra <- EL.consume
			return (x, extra))

test_Isolate_HandleEOF :: Suite
test_Isolate_HandleEOF = assertions "handle-eof" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(E.runLists_ [] $ do
			x <- ET.isolate 2 =$ ET.head
			extra <- EL.consume
			return (x, extra))

test_Isolate_BadParameter :: Suite
test_Isolate_BadParameter = assertions "bad-parameter" $ do
	$expect $ equal
		(Nothing, ["A", "B", "C"])
		(E.runLists_ [["A"], ["B"], ["C"]] $ do
			x <- ET.isolate 0 =$ ET.head
			extra <- EL.consume
			return (x, extra))

test_IsolateWhile :: Suite
test_IsolateWhile = suite "isolateWhile"
	[ test_IsolateWhile_DropExtra
	, test_IsolateWhile_HandleEOF
	]

test_IsolateWhile_DropExtra :: Suite
test_IsolateWhile_DropExtra = assertions "drop-extra" $ do
	$expect $ equal
		(Just 'A', ["C"])
		(E.runLists_ [[], ["A"], ["B"], ["C"]] $ do
			x <- ET.isolateWhile (< 'C') =$ ET.head
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 'A', ["C"])
		(E.runLists_ [["A", "B", "C"]] $ do
			x <- ET.isolateWhile (< 'C') =$ ET.head
			extra <- EL.consume
			return (x, extra))

test_IsolateWhile_HandleEOF :: Suite
test_IsolateWhile_HandleEOF = assertions "handle-eof" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(E.runLists_ [] $ do
			x <- ET.isolateWhile (< 'C') =$ ET.head
			extra <- EL.consume
			return (x, extra))
