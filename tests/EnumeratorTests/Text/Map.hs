{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Map
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
import qualified Data.Text as T

import           Test.Chell

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

test_Map :: Suite
test_Map = assertions "map" $ do
	$expect $ equal
		["a", "b"]
		(E.runLists_ [["AB"]] (ET.map toLower =$ EL.consume))
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- ET.map toLower =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapM :: Suite
test_MapM = assertions "mapM" $ do
	$expect $ equal
		["a", "b"]
		(E.runLists_ [["AB"]] (ET.mapM (return . toLower) =$ EL.consume))
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- ET.mapM (return . toLower) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapM_ :: Suite
test_MapM_ = assertions "mapM_" $ do
	$expect $ equal
		['A', 'B']
		(execWriter (E.run_ (E.enumLists [["AB"]] $$ ET.mapM_ (\x -> tell [x]))))

test_ConcatMap :: Suite
test_ConcatMap = assertions "concatMap" $ do
	$expect $ equal
		["Aa", "Bb"]
		(E.runLists_ [["AB"]] (ET.concatMap (\x -> T.pack [x, toLower x]) =$ EL.consume))
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- ET.concatMap (\x -> T.pack [x, toLower x]) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapM :: Suite
test_ConcatMapM = assertions "concatMapM" $ do
	$expect $ equal
		["Aa", "Bb"]
		(E.runLists_ [["AB"]] (ET.concatMapM (\x -> return (T.pack [x, toLower x])) =$ EL.consume))
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- ET.concatMapM (\x -> return (T.pack [x, toLower x])) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapAccum :: Suite
test_MapAccum = assertions "mapAccum" $ do
	let step s ao = (s + 1, chr (ord ao + s))
	$expect $ equal
		["B", "D", "F"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			ET.mapAccum step 1 =$ EL.consume)
	$expect $ equal
		("B", ["", "B", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- ET.mapAccum step 1 =$ ET.take 1
			extra <- EL.consume
			return (xs, extra))

test_MapAccumM :: Suite
test_MapAccumM = assertions "mapAccumM" $ do
	let step s ao = return (s + 1, chr (ord ao + s))
	$expect $ equal
		["B", "D", "F"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			ET.mapAccumM step 1 =$ EL.consume)
	$expect $ equal
		("B", ["", "B", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- ET.mapAccumM step 1 =$ ET.take 1
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapAccum :: Suite
test_ConcatMapAccum = assertions "concatMapAccum" $ do
	let step s ao = (s + 1, T.replicate s (T.pack [ao]))
	$expect $ equal
		["A", "BB", "CCC"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			ET.concatMapAccum step 1 =$ EL.consume)
	$expect $ equal
		("AB", ["", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- ET.concatMapAccum step 1 =$ ET.take 2
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapAccumM :: Suite
test_ConcatMapAccumM = assertions "concatMapAccumM" $ do
	let step s ao = return (s + 1, T.replicate s (T.pack [ao]))
	$expect $ equal
		["A", "BB", "CCC"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			ET.concatMapAccumM step 1 =$ EL.consume)
	$expect $ equal
		("AB", ["", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- ET.concatMapAccumM step 1 =$ ET.take 2
			extra <- EL.consume
			return (xs, extra))
