{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Map
	( test_Map
	, test_MapM
	, test_ConcatMap
	, test_ConcatMapM
	, test_MapAccum
	, test_MapAccumM
	) where

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_Map :: Suite
test_Map = test_Enumeratee "map" (EL.map id)

test_MapM :: Suite
test_MapM = test_Enumeratee "mapM" (EL.mapM return)

test_ConcatMap :: Suite
test_ConcatMap = test_Enumeratee "concatMap" (EL.concatMap (:[]))

test_ConcatMapM :: Suite
test_ConcatMapM = test_Enumeratee "concatMapM" (EL.concatMapM (\x -> return [x]))

test_MapAccum :: Suite
test_MapAccum = property "mapAccum" $ prop_List
	(do
		let enee = EL.mapAccum (\s ao -> (s+1, (s, ao))) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EL.consume
		return (a, b))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:xs') -> (Just (10, x), xs'))

test_MapAccumM :: Suite
test_MapAccumM = property "mapAccumM" $ prop_List
	(do
		let enee = EL.mapAccumM (\s ao -> return (s+1, (s, ao))) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EL.consume
		return (a, b))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:xs') -> (Just (10, x), xs'))
