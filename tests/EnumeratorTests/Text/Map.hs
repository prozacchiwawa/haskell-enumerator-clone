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
	, test_MapAccum
	, test_MapAccumM
	) where

import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Char (toLower)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util

test_Map :: Suite
test_Map = assertions "map" $ do
	$expect $ equal
		["a", "b"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ ET.map toLower =$ EL.consume)))
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- ET.map toLower =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

test_MapM :: Suite
test_MapM = assertions "mapM" $ do
	$expect $ equal
		["a", "b"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ ET.mapM (return . toLower) =$ EL.consume)))
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- ET.mapM (return . toLower) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

test_MapM_ :: Suite
test_MapM_ = assertions "mapM_" $ do
	$expect $ equal
		['A', 'B']
		(execWriter (E.run_ (E.enumList 1 ["AB"] $$ ET.mapM_ (\x -> tell [x]))))

test_ConcatMap :: Suite
test_ConcatMap = assertions "concatMap" $ do
	$expect $ equal
		["Aa", "Bb"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ ET.concatMap (\x -> T.pack [x, toLower x]) =$ EL.consume)))
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- ET.concatMap (\x -> T.pack [x, toLower x]) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

test_ConcatMapM :: Suite
test_ConcatMapM = assertions "concatMapM" $ do
	$expect $ equal
		["Aa", "Bb"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ ET.concatMapM (\x -> return (T.pack [x, toLower x])) =$ EL.consume)))
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- ET.concatMapM (\x -> return (T.pack [x, toLower x])) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

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
