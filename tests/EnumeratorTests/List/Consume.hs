{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Consume
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

import           EnumeratorTests.List.Util

test_Consume :: Suite
test_Consume = property "consume" (prop_List EL.consume Right)

test_Head :: Suite
test_Head = assertions "head" $ do
	$expect $ equal
		(Just 'A', ['B', 'C'])
		(runIdentity (E.run_ (E.enumList 2 ['A', 'B', 'C'] $$ do
			x <- EL.head
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(runIdentity (E.run_ (E.enumList 1 [] $$ do
			x <- EL.head
			extra <- EL.consume
			return (x, extra))))

test_Head_ :: Suite
test_Head_ = assertions "head_" $ do
	$expect $ equal
		('A', ['B', 'C'])
		(runIdentity (E.run_ (E.enumList 2 ['A', 'B', 'C'] $$ do
			x <- EL.head_
			extra <- EL.consume
			return (x, extra))))
	$expect $ throwsEq
		(ErrorCall "head_: stream has ended")
		(E.run_ (E.enumList 1 [] $$ do
			x <- EL.head_
			extra <- EL.consume
			return (x, extra)))

test_Take :: Suite
test_Take = assertions "take" $ do
	$expect $ equal
		(['A', 'B', 'C'], ['D', 'E'])
		(runIdentity (E.run_ (E.enumList 2 ['A'..'E'] $$ do
			x <- EL.take 3
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(['A', 'B'], [])
		(runIdentity (E.run_ (E.enumList 1 ['A', 'B'] $$ do
			x <- EL.take 3
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		([], ['A', 'B'])
		(runIdentity (E.run_ (E.enumList 1 ['A', 'B'] $$ do
			x <- EL.take 0
			extra <- EL.consume
			return (x, extra))))
