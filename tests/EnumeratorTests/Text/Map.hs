{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Map
	( test_Map
	, test_MapM
	, test_ConcatMap
	, test_ConcatMapM
	, test_MapAccum
	, test_MapAccumM
	) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Util (todo)
import           EnumeratorTests.Text.Util

test_Map :: Suite
test_Map = todo "map"

test_MapM :: Suite
test_MapM = todo "mapM"

test_ConcatMap :: Suite
test_ConcatMap = todo "concatMap"

test_ConcatMapM :: Suite
test_ConcatMapM = todo "concatMapM"

test_MapAccum :: Suite
test_MapAccum = property "mapAccum" $ prop_Text
	(do
		let enee = ET.mapAccum (\s ao -> (s+1, succ ao)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- ET.consume
		return (a, b))
	(\text -> Right $ case TL.uncons text of
		Nothing -> (Nothing, TL.empty)
		Just (c, text') -> (Just (T.singleton (succ c)), text'))

test_MapAccumM :: Suite
test_MapAccumM = property "mapAccumM" $ prop_Text
	(do
		let enee = ET.mapAccumM (\s ao -> return (s+1, succ ao)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- ET.consume
		return (a, b))
	(\text -> Right $ case TL.uncons text of
		Nothing -> (Nothing, TL.empty)
		Just (c, text') -> (Just (T.singleton (succ c)), text'))
