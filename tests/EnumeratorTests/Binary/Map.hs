{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Map
	( test_Map
	, test_MapM
	, test_ConcatMap
	, test_ConcatMapM
	, test_MapAccum
	, test_MapAccumM
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Util (todo)
import           EnumeratorTests.Binary.Util

test_Map :: Suite
test_Map = todo "map"

test_MapM :: Suite
test_MapM = todo "mapM"

test_ConcatMap :: Suite
test_ConcatMap = todo "concatMap"

test_ConcatMapM :: Suite
test_ConcatMapM = todo "concatMapM"

test_MapAccum :: Suite
test_MapAccum = property "mapAccum" $ prop_Bytes
	(do
		let enee = EB.mapAccum (\s ao -> (s+1, ao + s)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EB.consume
		return (a, b))
	(\bytes -> Right $ case BL.uncons bytes of
		Nothing -> (Nothing, BL.empty)
		Just (b, bytes') -> (Just (B.singleton (b + 10)), bytes'))

test_MapAccumM :: Suite
test_MapAccumM = property "mapAccumM" $ prop_Bytes
	(do
		let enee = EB.mapAccumM (\s ao -> return (s+1, ao + s)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EB.consume
		return (a, b))
	(\bytes -> Right $ case BL.uncons bytes of
		Nothing -> (Nothing, BL.empty)
		Just (b, bytes') -> (Just (B.singleton (b + 10)), bytes'))
