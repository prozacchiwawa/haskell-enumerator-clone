{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Consume
	( test_Consume
	, test_Head
	, test_Head_
	, test_Take
	) where

import           Control.Exception
import           Data.Functor.Identity (runIdentity)
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util (prop_Text)

test_Consume :: Suite
test_Consume = property "consume" (prop_Text ET.consume Right)

test_Head :: Suite
test_Head = assertions "head" $ do
	$expect $ equal
		(Just 'A', ["BC", "DE"])
		(runIdentity (E.run_ (E.enumList 2 ["ABC", "DE"] $$ do
			x <- ET.head
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(runIdentity (E.run_ (E.enumList 1 [] $$ do
			x <- ET.head
			extra <- EL.consume
			return (x, extra))))

test_Head_ :: Suite
test_Head_ = assertions "head_" $ do
	$expect $ equal
		('A', ["BC", "DE"])
		(runIdentity (E.run_ (E.enumList 1 ["ABC", "DE"] $$ do
			x <- ET.head_
			extra <- EL.consume
			return (x, extra))))
	$expect $ throwsEq
		(ErrorCall "head_: stream has ended")
		(E.run_ (E.enumList 1 [] $$ do
			x <- ET.head_
			extra <- EL.consume
			return (x, extra)))

test_Take :: Suite
test_Take = assertions "take" $ do
	$expect $ equal
		("ABC", ["D", "E"])
		(runIdentity (E.run_ (E.enumList 2 ["A", "B", "C", "D", "E"] $$ do
			x <- ET.take 3
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		("AB", [])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B"] $$ do
			x <- ET.take 3
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		("", ["A", "B"])
		(runIdentity (E.run_ (E.enumList 1 ["A", "B"] $$ do
			x <- ET.take 0
			extra <- EL.consume
			return (x, extra))))
