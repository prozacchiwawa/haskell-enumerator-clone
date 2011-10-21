{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Consume
	( test_Consume
	, test_Head
	, test_Head_
	, test_Take
	, test_TakeWhile
	) where

import           Control.Exception
import           Test.Chell

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util (equalExc)

test_Consume :: Suite
test_Consume = assertions "consume" $ do
	$expect $ equal
		(['A', 'B', 'C'], Nothing)
		(E.runLists_ [[], ['A', 'B'], ['C']] $ do
			xs <- EL.consume
			h <- EL.head
			return (xs, h))

test_Head :: Suite
test_Head = assertions "head" $ do
	$expect $ equal
		(Just 'A', ['B', 'C'])
		(E.runLists_ [[], ['A', 'B'], ['C']] $ do
			x <- EL.head
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(E.runLists_ [] $ do
			x <- EL.head
			extra <- EL.consume
			return (x, extra))

test_Head_ :: Suite
test_Head_ = assertions "head_" $ do
	$expect $ equal
		('A', ['B', 'C'])
		(E.runLists_ [[], ['A', 'B'], ['C']] $ do
			x <- EL.head_
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "head_: stream has ended")
		(E.runLists [] $ do
			x <- EL.head_
			extra <- EL.consume
			return (x, extra))

test_Take :: Suite
test_Take = assertions "take" $ do
	$expect $ equal
		(['A', 'B', 'C'], ['D', 'E'])
		(E.runLists_ [['A', 'B'], ['C', 'D'], ['E']] $ do
			x <- EL.take 3
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(['A', 'B'], [])
		(E.runLists_ [['A'], ['B']] $ do
			x <- EL.take 3
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		([], ['A', 'B'])
		(E.runLists_ [['A'], ['B']] $ do
			x <- EL.take 0
			extra <- EL.consume
			return (x, extra))

test_TakeWhile :: Suite
test_TakeWhile = assertions "takeWhile" $ do
	$expect $ equal
		(['A', 'B', 'C'], ['D', 'E'])
		(E.runLists_ [[], ['A', 'B'], ['C', 'D'], ['E']] $ do
			x <- EL.takeWhile (< 'D')
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(['A', 'B'], [])
		(E.runLists_ [['A'], ['B']] $ do
			x <- EL.takeWhile (< 'D')
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		([], ['A', 'B'])
		(E.runLists_ [['A'], ['B']] $ do
			x <- EL.takeWhile (< 'A')
			extra <- EL.consume
			return (x, extra))
