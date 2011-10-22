{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Map
	( test_Map
	, test_MapM
	, test_MapM_
	, test_ConcatMap
	, test_ConcatMapM
	, test_ConcatMapAccum
	, test_ConcatMapAccumM
	, test_MapAccum
	, test_MapAccumM
	) where

import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Char (chr, ord, toLower)
import           Test.Chell

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_Map :: Suite
test_Map = assertions "map" $ do
	$expect $ equal
		['a', 'b', 'c']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.map toLower =$ EL.consume)
	$expect $ equal
		(['a'], ['B', 'C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.map toLower =$ EL.take 1
			extra <- EL.consume
			return (xs, extra))

test_MapM :: Suite
test_MapM = assertions "mapM" $ do
	let step = return . toLower
	$expect $ equal
		['a', 'b', 'c']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.mapM step =$ EL.consume)
	$expect $ equal
		(['a'], ['B', 'C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.mapM step =$ EL.take 1
			extra <- EL.consume
			return (xs, extra))

test_MapM_ :: Suite
test_MapM_ = assertions "mapM_" $ do
	$expect $ equal
		['A', 'B', 'C']
		(execWriter (E.run_ (E.enumList 1 ['A', 'B', 'C'] $$ EL.mapM_ (\x -> tell [x]))))

test_ConcatMap :: Suite
test_ConcatMap = assertions "concatMap" $ do
	let step ao = [ao, toLower ao]
	$expect $ equal
		['A', 'a', 'B', 'b', 'C', 'c']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.concatMap step =$ EL.consume)
	$expect $ equal
		(['A', 'a'], ['B', 'C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.concatMap step =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapM :: Suite
test_ConcatMapM = assertions "concatMapM" $ do
	let step ao = return [ao, toLower ao]
	$expect $ equal
		['A', 'a', 'B', 'b', 'C', 'c']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.concatMapM step =$ EL.consume)
	$expect $ equal
		(['A', 'a'], ['B', 'C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.concatMapM step =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapAccum :: Suite
test_MapAccum = assertions "mapAccum" $ do
	let step s ao = (s + 1, chr (ord ao + s))
	$expect $ equal
		['B', 'D', 'F']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.mapAccum step 1 =$ EL.consume)
	$expect $ equal
		(['B'], ['B', 'C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.mapAccum step 1 =$ EL.take 1
			extra <- EL.consume
			return (xs, extra))

test_MapAccumM :: Suite
test_MapAccumM = assertions "mapAccumM" $ do
	let step s ao = return (s + 1, chr (ord ao + s))
	$expect $ equal
		['B', 'D', 'F']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.mapAccumM step 1 =$ EL.consume)
	$expect $ equal
		(['B'], ['B', 'C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.mapAccumM step 1 =$ EL.take 1
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapAccum :: Suite
test_ConcatMapAccum = assertions "concatMapAccum" $ do
	let step s ao = (s + 1, replicate s ao)
	$expect $ equal
		['A', 'B', 'B', 'C', 'C', 'C']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.concatMapAccum step 1 =$ EL.consume)
	$expect $ equal
		(['A', 'B'], ['C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.concatMapAccum step 1 =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapAccumM :: Suite
test_ConcatMapAccumM = assertions "concatMapAccumM" $ do
	let step s ao = return (s + 1, replicate s ao)
	$expect $ equal
		['A', 'B', 'B', 'C', 'C', 'C']
		(E.runLists_ [['A', 'B'], ['C']] $ do
			EL.concatMapAccumM step 1 =$ EL.consume)
	$expect $ equal
		(['A', 'B'], ['C'])
		(E.runLists_ [['A', 'B'], ['C']] $ do
			xs <- EL.concatMapAccumM step 1 =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))
