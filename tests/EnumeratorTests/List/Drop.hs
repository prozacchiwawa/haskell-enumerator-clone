{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Drop
	( test_Drop
	, test_DropWhile
	, test_Filter
	, test_FilterM
	) where

import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_Drop :: Suite
test_Drop = assertions "drop" $ do
	$expect $ equal
		['A', 'B', 'C', 'D', 'E']
		(runIdentity (E.run_ (E.enumList 1 ['A'..'E'] $$ EL.drop 0 >> EL.consume)))
	$expect $ equal
		['C', 'D', 'E']
		(runIdentity (E.run_ (E.enumList 1 ['A'..'E'] $$ EL.drop 2 >> EL.consume)))

test_DropWhile :: Suite
test_DropWhile = assertions "dropWhile" $ do
	$expect $ equal
		['C', 'D', 'E']
		(runIdentity (E.run_ (E.enumList 1 ['A'..'E'] $$ EL.dropWhile (< 'C') >> EL.consume)))
	$expect $ equal
		[]
		(runIdentity (E.run_ (E.enumList 1 ['A'..'E'] $$ EL.dropWhile (\_ -> True) >> EL.consume)))

test_Filter :: Suite
test_Filter = assertions "filter" $ do
	$expect $ equal
		['A', 'B', 'D', 'E']
		(runIdentity (E.run_ (E.enumList 1 ['A'..'E'] $$ E.joinI (EL.filter (/= 'C') $$ EL.consume))))

test_FilterM :: Suite
test_FilterM = assertions "filterM" $ do
	$expect $ equal
		['A', 'B', 'D', 'E']
		(runIdentity (E.run_ (E.enumList 1 ['A'..'E'] $$ E.joinI (EL.filterM (\x -> return (x /= 'C')) $$ EL.consume))))

