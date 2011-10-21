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
	, test_MapAccum
	, test_MapAccumM
	) where

import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (runIdentity)
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Binary.Util

test_Map :: Suite
test_Map = assertions "map" $ do
	$expect $ equal
		["a", "b"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ EB.map (+ 0x20) =$ EL.consume)))
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- EB.map (+ 0x20) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

test_MapM :: Suite
test_MapM = assertions "mapM" $ do
	$expect $ equal
		["a", "b"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ EB.mapM (\x -> return (x + 0x20)) =$ EL.consume)))
	$expect $ equal
		(["a", "b"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- EB.mapM (\x -> return (x + 0x20)) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

test_MapM_ :: Suite
test_MapM_ = assertions "mapM_" $ do
	$expect $ equal
		[0x41, 0x42]
		(execWriter (E.run_ (E.enumList 1 ["AB"] $$ EB.mapM_ (\x -> tell [x]))))

test_ConcatMap :: Suite
test_ConcatMap = assertions "concatMap" $ do
	$expect $ equal
		["Aa", "Bb"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ EB.concatMap (\x -> B.pack [x, x + 0x20]) =$ EL.consume)))
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- EB.concatMap (\x -> B.pack [x, x + 0x20]) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

test_ConcatMapM :: Suite
test_ConcatMapM = assertions "concatMapM" $ do
	$expect $ equal
		["Aa", "Bb"]
		(runIdentity (E.run_ (E.enumList 1 ["AB"] $$ EB.concatMapM (\x -> return (B.pack [x, x + 0x20])) =$ EL.consume)))
	$expect $ equal
		(["Aa", "Bb"], ["CDEF", "GH"])
		(runIdentity (E.run_ (E.enumList 2 ["ABCD", "EF", "GH"] $$ do
			xs <- EB.concatMapM (\x -> return (B.pack [x, x + 0x20])) =$ EL.take 2
			extra <- EL.consume
			return (xs, extra))))

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
