{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Require
	( test_Require
	) where

import qualified Control.Exception as Exc
import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util (prop_ListN)
import           EnumeratorTests.Util (equalExc)

test_Require :: Suite
test_Require = suite "require"
	[ prop_Require
	, test_YieldsInput
	, test_HandleEOF
	, test_BadParameter
	]

prop_Require :: Suite
prop_Require = property "model" $ prop_ListN
	(\n -> do
		EL.require n
		EL.consume)
	(\n xs -> if n > toInteger (length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)

test_YieldsInput :: Suite
test_YieldsInput = assertions "yields-input" $ do
	$expect $ equal
		['A', 'B', 'C']
		(E.runLists_ [['A'], ['B'], ['C']] $ do
			EL.require 2
			EL.consume)
	$expect $ equal
		['A', 'B', 'C']
		(E.runLists_ [['A', 'B', 'C']] $ do
			EL.require 2
			EL.consume)

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	$expect $ equalExc
		(Exc.ErrorCall "require: Unexpected EOF")
		(E.runLists [] $ do
			EL.require 2
			EL.consume)

test_BadParameter :: Suite
test_BadParameter = assertions "bad-parameter" $ do
	$expect $ equal
		([] :: [Char])
		(E.runLists_ [] $ do
			EL.require 0
			EL.consume)
