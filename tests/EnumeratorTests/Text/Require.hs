{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Require
	( test_Require
	) where

import qualified Control.Exception as Exc
import qualified Data.Text.Lazy as TL
import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util
import           EnumeratorTests.Util (equalExc)

test_Require :: Suite
test_Require = suite "require"
	[ prop_Require
	, test_YieldsInput
	, test_HandleEOF
	, test_BadParameter
	]

prop_Require :: Suite
prop_Require = property "model" $ prop_TextN
	(\n -> do
		ET.require n
		ET.consume)
	(\n xs -> if n > toInteger (TL.length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)

test_YieldsInput :: Suite
test_YieldsInput = assertions "yields-input" $ do
	$expect $ equal
		["A", "B", "C"]
		(E.runLists_ [["A"], ["B"], ["C"]] $ do
			ET.require 2
			EL.consume)
	$expect $ equal
		["A", "B", "C"]
		(E.runLists_ [["A", "B", "C"]] $ do
			ET.require 2
			EL.consume)

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	$expect $ equalExc
		(Exc.ErrorCall "require: Unexpected EOF")
		(E.runLists [] $ do
			ET.require 2
			EL.consume)

test_BadParameter :: Suite
test_BadParameter = assertions "bad-parameter" $ do
	$expect $ equal
		[]
		(E.runLists_ [] $ do
			ET.require 0
			EL.consume)
