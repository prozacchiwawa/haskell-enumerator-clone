{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Zip
	( test_Zip
	) where

import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_Zip :: Suite
test_Zip = assertions "zip" $ do
	let iterTup = do
		Just x <- EL.head
		Just y <- EL.head
		return (x, y)
	let iterTupFlip = do
		Just x <- EL.head
		Just y <- EL.head
		return (y, x)
	
	let check i1 i2 = E.run_ (E.enumList 4 [1, 2, 3, 4, 5] $$ do
		(x, y) <- EL.zip i1 i2
		extra <- EL.consume
		return (x, y, extra))
	
	-- Both sides have same behavior
	(tup, tup2, extra) <- check iterTup iterTupFlip
	$expect (equal tup (1, 2))
	$expect (equal tup2 (2, 1))
	$expect (equal extra [3, 4, 5])
	
	-- First side has more extra data
	(took, tup, extra) <- check (EL.take 1) iterTup
	$expect (equal took [1])
	$expect (equal tup (1, 2))
	$expect (equal extra [3, 4, 5])
	
	-- Second side has more extra data
	(tup, took, extra) <- check iterTup (EL.take 1)
	$expect (equal tup (1, 2))
	$expect (equal took [1])
	$expect (equal extra [3, 4, 5])
