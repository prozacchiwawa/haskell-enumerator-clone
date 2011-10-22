{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Require
	( test_Require
	) where

import qualified Control.Exception as Exc
import qualified Data.ByteString.Lazy as BL
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Binary.Util

test_Require :: Suite
test_Require = suite "require"
	[ prop_Require
	, test_YieldsInput
	, test_HandleEOF
	, test_BadParameter
	]

prop_Require :: Suite
prop_Require = property "model" $ prop_BytesN
	(\n -> do
		EB.require n
		EB.consume)
	(\n xs -> if n > toInteger (BL.length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)

test_YieldsInput :: Suite
test_YieldsInput = assertions "yields-input" $ do
	$expect $ equal
		["A", "B", "C"]
		(E.runLists_ [["A"], ["B"], ["C"]] $ do
			EB.require 2
			EL.consume)
	$expect $ equal
		["A", "B", "C"]
		(E.runLists_ [["A", "B", "C"]] $ do
			EB.require 2
			EL.consume)

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	$expect $ throwsEq
		(Exc.ErrorCall "require: Unexpected EOF")
		(E.run_ (E.enumLists [] $$ do
			EB.require 2
			EL.consume))

test_BadParameter :: Suite
test_BadParameter = assertions "bad-parameter" $ do
	$expect $ equal
		[]
		(E.runLists_ [] $ do
			EB.require 0
			EL.consume)
