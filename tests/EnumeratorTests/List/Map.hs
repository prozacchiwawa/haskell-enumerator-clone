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
	, test_MapAccum
	, test_MapAccumM
	) where

import           Control.Monad.Trans.Writer
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

test_MapM_ :: Suite
test_MapM_ = assertions "mapM_" $ do
	$expect $ equal
		['A', 'B', 'C']
		(execWriter (E.run_ (E.enumList 1 ['A', 'B', 'C'] $$ EL.mapM_ (\x -> tell [x]))))

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
