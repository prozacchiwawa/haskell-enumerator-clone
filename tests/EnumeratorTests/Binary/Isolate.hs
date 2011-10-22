{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Isolate
	( test_Isolate
	) where

import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word8)

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Binary.Util (prop_Bytes)

test_Isolate :: Suite
test_Isolate = suite "isolate"
	[ prop_Isolate
	, test_DropExtra
	, test_HandleEOF
	, test_BadParameter
	]

prop_Isolate :: Suite
prop_Isolate = property "model" $ prop_Bytes
	(do
		x <- E.joinI (EB.isolate 2 $$ EB.head)
		extra <- EB.consume
		return (x, extra))
	(\bytes -> Right $ case BL.unpack bytes of
		[] -> (Nothing, BL.empty)
		(x:[]) -> (Just x, BL.empty)
		(x:_:xs) -> (Just x, BL.pack xs))

test_DropExtra :: Suite
test_DropExtra = assertions "drop-extra" $ do
	$expect $ equal
		(Just 0x41, ["C"])
		(E.runLists_ [[], ["A"], ["B"], ["C"]] $ do
			x <- EB.isolate 2 =$ EB.head
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 0x41, ["C"])
		(E.runLists_ [["A", "B", "C"]] $ do
			x <- EB.isolate 2 =$ EB.head
			extra <- EL.consume
			return (x, extra))

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	$expect $ equal
		(Nothing :: Maybe Word8, [])
		(E.runLists_ [] $ do
			x <- EB.isolate 2 =$ EB.head
			extra <- EL.consume
			return (x, extra))

test_BadParameter :: Suite
test_BadParameter = assertions "bad-parameter" $ do
	$expect $ equal
		(Nothing, ["A", "B", "C"])
		(E.runLists_ [["A"], ["B"], ["C"]] $ do
			x <- EB.isolate 0 =$ EB.head
			extra <- EL.consume
			return (x, extra))
