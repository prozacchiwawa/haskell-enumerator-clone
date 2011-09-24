{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Consume
	( test_Consume
	, test_Head
	, test_Take
	) where

import           Data.List (genericSplitAt)

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_Consume :: Suite
test_Consume = property "consume" (prop_List EL.consume Right)

test_Head :: Suite
test_Head = property "head" $ prop_List
	(do
		x <- EL.head
		extra <- EL.consume
		return (x, extra)
	)
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:xs') -> (Just x, xs'))

test_Take :: Suite
test_Take = property "take" $ prop_ListN
	(\n -> do
		xs <- EL.take n
		extra <- EL.consume
		return (xs, extra))
	(\n -> Right . genericSplitAt n)
