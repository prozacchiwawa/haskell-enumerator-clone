{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Map
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
import qualified Data.ByteString as B
import           Test.Chell

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

test_Map :: Suite
test_Map = assertions "map" $ do
	$expect $ equal
		["a", "b"]
		(E.runLists_ [["AB"]] $ do
			EB.map (+ 0x20) =$ EL.consume)
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- EB.map (+ 0x20) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapM :: Suite
test_MapM = assertions "mapM" $ do
	$expect $ equal
		["a", "b"]
		(E.runLists_ [["AB"]] $ do
			EB.mapM (\x -> return (x + 0x20)) =$ EL.consume)
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- EB.mapM (\x -> return (x + 0x20)) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapM_ :: Suite
test_MapM_ = assertions "mapM_" $ do
	$expect $ equal
		[0x41, 0x42]
		(execWriter (E.run_ (E.enumLists [["AB"]] $$ EB.mapM_ (\x -> tell [x]))))

test_ConcatMap :: Suite
test_ConcatMap = assertions "concatMap" $ do
	$expect $ equal
		["Aa", "Bb"]
		(E.runLists_ [["AB"]] $ do
			EB.concatMap (\x -> B.pack [x, x + 0x20]) =$ EL.consume)
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- EB.concatMap (\x -> B.pack [x, x + 0x20]) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapM :: Suite
test_ConcatMapM = assertions "concatMapM" $ do
	$expect $ equal
		["Aa", "Bb"]
		(E.runLists_ [["AB"]] $ do
			EB.concatMapM (\x -> return (B.pack [x, x + 0x20])) =$ EL.consume)
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(E.runLists_ [["ABCD", "EF"], ["GH"]] $ do
			xs <- EB.concatMapM (\x -> return (B.pack [x, x + 0x20])) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))

test_MapAccum :: Suite
test_MapAccum = assertions "mapAccum" $ do
	let step s ao = (s + 1, ao + s)
	$expect $ equal
		["B", "D", "F"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			EB.mapAccum step 1 =$ EL.consume)
	$expect $ equal
		("B", ["", "B", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- EB.mapAccum step 1 =$ EB.take 1
			extra <- EL.consume
			return (xs, extra))

test_MapAccumM :: Suite
test_MapAccumM = assertions "mapAccumM" $ do
	let step s ao = return (s + 1, ao + s)
	$expect $ equal
		["B", "D", "F"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			EB.mapAccumM step 1 =$ EL.consume)
	$expect $ equal
		("B", ["", "B", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- EB.mapAccumM step 1 =$ EB.take 1
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapAccum :: Suite
test_ConcatMapAccum = assertions "concatMapAccum" $ do
	let step s ao = (s + 1, B.replicate s ao)
	$expect $ equal
		["A", "BB", "CCC"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			EB.concatMapAccum step 1 =$ EL.consume)
	$expect $ equal
		("AB", ["", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- EB.concatMapAccum step 1 =$ EB.take 2
			extra <- EL.consume
			return (xs, extra))

test_ConcatMapAccumM :: Suite
test_ConcatMapAccumM = assertions "concatMapAccumM" $ do
	let step s ao = return (s + 1, B.replicate s ao)
	$expect $ equal
		["A", "BB", "CCC"]
		(E.runLists_ [["A", "B"], ["C"]] $ do
			EB.concatMapAccumM step 1 =$ EL.consume)
	$expect $ equal
		("AB", ["", "C"])
		(E.runLists_ [["A", "B"], ["C"]] $ do
			xs <- EB.concatMapAccumM step 1 =$ EB.take 2
			extra <- EL.consume
			return (xs, extra))
