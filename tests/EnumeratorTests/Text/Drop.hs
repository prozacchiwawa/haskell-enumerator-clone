{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Drop
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
import qualified Data.Enumerator.Text as ET

test_Drop :: Suite
test_Drop = assertions "drop" $ do
	$expect $ equal
		["ABCDE"]
		(runIdentity (E.run_ (E.enumList 1 ["ABCDE"] $$ ET.drop 0 >> EL.consume)))
	$expect $ equal
		["CDE"]
		(runIdentity (E.run_ (E.enumList 1 ["ABCDE"] $$ ET.drop 2 >> EL.consume)))
	$expect $ equal
		["CDE"]
		(runIdentity (E.run_ (E.enumList 1 ["A", "BCDE"] $$ ET.drop 2 >> EL.consume)))

test_DropWhile :: Suite
test_DropWhile = assertions "dropWhile" $ do
	$expect $ equal
		["CDE"]
		(runIdentity (E.run_ (E.enumList 1 ["ABCDE"] $$ ET.dropWhile (< 'C') >> EL.consume)))
	$expect $ equal
		[]
		(runIdentity (E.run_ (E.enumList 1 ["ABCDE"] $$ ET.dropWhile (\_ -> True) >> EL.consume)))

test_Filter :: Suite
test_Filter = assertions "filter" $ do
	$expect $ equal
		["A", "B", "", "D", "E"]
		(runIdentity (E.run_ (E.enumList 1 ["ABCDE"] $$ E.joinI (ET.filter (/= 'C') $$ EL.consume))))

test_FilterM :: Suite
test_FilterM = assertions "filterM" $ do
	$expect $ equal
		["A", "B", "", "D", "E"]
		(runIdentity (E.run_ (E.enumList 1 ["ABCDE"] $$ E.joinI (ET.filterM (\x -> return (x /= 'C')) $$ EL.consume))))

