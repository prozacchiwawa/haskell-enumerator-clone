{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Consume
	( test_Consume
	, test_Head
	, test_Head_
	, test_Take
	) where

import           Control.Exception
import           Data.Functor.Identity (runIdentity)
import           Data.Word (Word8)
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Binary.Util (prop_Bytes)

test_Consume :: Suite
test_Consume = property "consume" (prop_Bytes EB.consume Right)

test_Head :: Suite
test_Head = assertions "head" $ do
	$expect $ equal
		(Just 0x41, ["BC", "DE"])
		(runIdentity (E.run_ (E.enumList 2 ["ABC", "DE"] $$ do
			x <- EB.head
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(Nothing :: Maybe Word8, [])
		(runIdentity (E.run_ (E.enumList 1 [] $$ do
			x <- EB.head
			extra <- EL.consume
			return (x, extra))))

test_Head_ :: Suite
test_Head_ = assertions "head_" $ do
	$expect $ equal
		(0x41, ["BC", "DE"])
		(runIdentity (E.run_ (E.enumList 1 ["ABC", "DE"] $$ do
			x <- EB.head_
			extra <- EL.consume
			return (x, extra))))
	$expect $ throwsEq
		(ErrorCall "head_: stream has ended")
		(E.run_ (E.enumList 1 [] $$ do
			x <- EB.head_
			extra <- EL.consume
			return (x, extra)))

test_Take :: Suite
test_Take = assertions "take" $ do
	$expect $ equal
		("ABC", ["D", "E"])
		(runIdentity (E.run_ (E.enumList 2 ["A", "B", "C", "D", "E"] $$ do
			x <- EB.take 3
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		("AB", [])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B"] $$ do
			x <- EB.take 3
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		("", ["A", "B"])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B"] $$ do
			x <- EB.take 0
			extra <- EL.consume
			return (x, extra))))
