{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Zip
	( test_Zip
	) where

import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

test_Zip :: Suite
test_Zip = assertions "zip" $ do
	let iterTup = do
		Just x <- EB.head
		Just y <- EB.head
		return (x, y)
	let iterTupFlip = do
		Just x <- EB.head
		Just y <- EB.head
		return (y, x)
	
	let check i1 i2 = E.run_ (E.enumList 2 ["abc", "def", "ghi"] $$ do
		(x, y) <- EB.zip i1 i2
		extra <- EL.consume
		return (x, y, extra))
	
	-- Both sides have same behavior
	(tup, tup2, extra) <- check iterTup iterTupFlip
	$expect (equal tup (0x61, 0x62))
	$expect (equal tup2 (0x62, 0x61))
	$expect (equal extra ["c", "def", "ghi"])
	
	-- First side has more extra data
	(took, tup, extra) <- check (EB.take 1) iterTup
	$expect (equal took "a")
	$expect (equal tup (0x61, 0x62))
	$expect (equal extra ["c", "def", "ghi"])
	
	-- Second side has more extra data
	(tup, took, extra) <- check iterTup (EB.take 1)
	$expect (equal tup (0x61, 0x62))
	$expect (equal took "a")
	$expect (equal extra ["c", "def", "ghi"])
