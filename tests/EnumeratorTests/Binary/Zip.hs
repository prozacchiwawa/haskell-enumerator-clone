{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Zip
	( test_Zip
	) where

import qualified Control.Exception as Exc
import           Data.ByteString (ByteString)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Text (Text)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util ()

test_ZipN :: (Eq b, Show b) => Text -> E.Iteratee ByteString Identity b -> b -> Suite
test_ZipN name iter expected = assertions name $ do
	$expect $ equal
		expected
		(runIdentity (E.run_ (E.enumList 1 ["A", "B"] $$ iter)))

$([d||])

test_Zip :: Suite
test_Zip = suite "zip"
	[ test_ContinueContinue
	, test_YieldContinue
	, test_ContinueYield
	, test_YieldYield
	, test_ErrorFirst
	, test_ErrorSecond
	, test_HandleEOF
	, test_Zip3
	, test_Zip4
	, test_Zip5
	, test_Zip6
	, test_Zip7
	, test_ZipWith
	, test_ZipWith3
	, test_ZipWith4
	, test_ZipWith5
	, test_ZipWith6
	, test_ZipWith7
	]

test_ContinueContinue :: Suite
test_ContinueContinue = assertions "continue-continue" $ do
	$expect $ equal
		("AB", "AB", ["C"])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B", "C"] $$ do
			(x, y) <- EB.zip (EB.take 2) (EB.take 2)
			extra <- EL.consume
			return (x, y, extra))))

test_YieldContinue :: Suite
test_YieldContinue = assertions "yield-continue" $ do
	$expect $ equal
		("A", "AB", ["C"])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B", "C"] $$ do
			(x, y) <- EB.zip (EB.take 1) (EB.take 2)
			extra <- EL.consume
			return (x, y, extra))))

test_ContinueYield :: Suite
test_ContinueYield = assertions "continue-yield" $ do
	$expect $ equal
		("AB", "A", ["C"])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B", "C"] $$ do
			(x, y) <- EB.zip (EB.take 2) (EB.take 1)
			extra <- EL.consume
			return (x, y, extra))))

test_YieldYield :: Suite
test_YieldYield = assertions "yield-yield" $ do
	$expect $ equal
		("A", "A", ["B", "C"])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B", "C"] $$ do
			(x, y) <- EB.zip (EB.take 1) (EB.take 1)
			extra <- EL.consume
			return (x, y, extra))))

test_ErrorFirst :: Suite
test_ErrorFirst = assertions "error-first" $ do
	$expect $ throwsEq
		(Exc.ErrorCall "error")
		(E.run_ (E.enumList 1 ["A", "B", "C"] $$ EB.zip (E.throwError (Exc.ErrorCall "error")) (EB.take 1)))

test_ErrorSecond :: Suite
test_ErrorSecond = assertions "error-second" $ do
	$expect $ throwsEq
		(Exc.ErrorCall "error")
		(E.run_ (E.enumList 1 ["A", "B", "C"] $$ EB.zip (EB.take 1) (E.throwError (Exc.ErrorCall "error"))))

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	$expect $ equal
		("A", "AB", [])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B"] $$ do
			(x, y) <- EB.zip (EB.take 1) (EB.take 3)
			extra <- EL.consume
			return (x, y, extra))))

test_Zip3 :: Suite
test_Zip3 = test_ZipN "zip3"
	(EB.zip3 EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41)

test_Zip4 :: Suite
test_Zip4 = test_ZipN "zip4"
	(EB.zip4 EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_Zip5 :: Suite
test_Zip5 = test_ZipN "zip5"
	(EB.zip5 EB.head EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_Zip6 :: Suite
test_Zip6 = test_ZipN "zip6"
	(EB.zip6 EB.head EB.head EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_Zip7 :: Suite
test_Zip7 = test_ZipN "zip7"
	(EB.zip7 EB.head EB.head EB.head EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_ZipWith :: Suite
test_ZipWith = test_ZipN "zipWith"
	(EB.zipWith (,) EB.head EB.head)
	(Just 0x41, Just 0x41)

test_ZipWith3 :: Suite
test_ZipWith3 = test_ZipN "zipWith3"
	(EB.zipWith3 (,,) EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41)

test_ZipWith4 :: Suite
test_ZipWith4 = test_ZipN "zipWith4"
	(EB.zipWith4 (,,,) EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_ZipWith5 :: Suite
test_ZipWith5 = test_ZipN "zipWith5"
	(EB.zipWith5 (,,,,) EB.head EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_ZipWith6 :: Suite
test_ZipWith6 = test_ZipN "zipWith6"
	(EB.zipWith6 (,,,,,) EB.head EB.head EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41)

test_ZipWith7 :: Suite
test_ZipWith7 = test_ZipN "zipWith7"
	(EB.zipWith7 (,,,,,,) EB.head EB.head EB.head EB.head EB.head EB.head EB.head)
	(Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41, Just 0x41)
