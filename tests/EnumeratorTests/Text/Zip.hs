{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Zip
	( test_Zip
	) where

import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

test_Zip :: Suite
test_Zip = test $ assertions "zip" $ do
	let iterTup = do
		Just x <- ET.head
		Just y <- ET.head
		return (x, y)
	let iterTupFlip = do
		Just x <- ET.head
		Just y <- ET.head
		return (y, x)
	
	let check i1 i2 = E.run_ (E.enumList 2 ["abc", "def", "ghi"] $$ do
		(x, y) <- ET.zip i1 i2
		extra <- EL.consume
		return (x, y, extra))
	
	-- Both sides have same behavior
	(tup, tup2, extra) <- check iterTup iterTupFlip
	$expect (equal tup ('a', 'b'))
	$expect (equal tup2 ('b', 'a'))
	$expect (equal extra ["c", "def", "ghi"])
	
	-- First side has more extra data
	(took, tup, extra) <- check (ET.take 1) iterTup
	$expect (equal took "a")
	$expect (equal tup ('a', 'b'))
	$expect (equal extra ["c", "def", "ghi"])
	
	-- Second side has more extra data
	(tup, took, extra) <- check iterTup (ET.take 1)
	$expect (equal tup ('a', 'b'))
	$expect (equal took "a")
	$expect (equal extra ["c", "def", "ghi"])
