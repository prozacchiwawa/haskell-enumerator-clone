{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Consume
	( test_Consume
	, test_Head
	, test_Head_
	, test_Take
	, test_TakeWhile
	) where

import           Control.Exception
import           Data.Word (Word8)
import           Test.Chell

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Util (equalExc)

test_Consume :: Suite
test_Consume = assertions "consume" $ do
	$expect $ equal
		("ABC", Nothing)
		(E.runLists_ [[], ["A", "B"], ["C"]] $ do
			xs <- EB.consume
			h <- EL.head
			return (xs, h))

test_Head :: Suite
test_Head = assertions "head" $ do
	$expect $ equal
		(Just 0x41, ["BC", "DE"])
		(E.runLists_ [[], ["ABC", "DE"]] $ do
			x <- EB.head
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Nothing :: Maybe Word8, [])
		(E.runLists_ [] $ do
			x <- EB.head
			extra <- EL.consume
			return (x, extra))

test_Head_ :: Suite
test_Head_ = assertions "head_" $ do
	$expect $ equal
		(0x41, ["BC", "DE"])
		(E.runLists_ [["ABC"], ["DE"]] $ do
			x <- EB.head_
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "head_: stream has ended")
		(E.runLists [] $ do
			x <- EB.head_
			extra <- EL.consume
			return (x, extra))

test_Take :: Suite
test_Take = assertions "take" $ do
	$expect $ equal
		("ABC", ["D", "E"])
		(E.runLists_ [["A", "B"], ["C", "D"], ["E"]] $ do
			x <- EB.take 3
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		("AB", [])
		(E.runLists_ [["A"], ["B"]] $ do
			x <- EB.take 3
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		("", ["A", "B"])
		(E.runLists_ [["A"], ["B"]] $ do
			x <- EB.take 0
			extra <- EL.consume
			return (x, extra))

test_TakeWhile :: Suite
test_TakeWhile = assertions "takeWhile" $ do
	$expect $ equal
		("ABC", ["D", "E"])
		(E.runLists_ [[], ["A", "B"], ["C", "D"], ["E"]] $ do
			x <- EB.takeWhile (< 0x44)
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		("AB", [])
		(E.runLists_ [["A"], ["B"]] $ do
			x <- EB.takeWhile (< 0x44)
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		("", ["A", "B"])
		(E.runLists_ [["A"], ["B"]] $ do
			x <- EB.takeWhile (< 0x41)
			extra <- EL.consume
			return (x, extra))
