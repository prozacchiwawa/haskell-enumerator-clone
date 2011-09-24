{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Consume
	( test_Consume
	, test_Head
	, test_Take
	) where

import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util

test_Consume :: Suite
test_Consume = property "consume" (prop_Text ET.consume Right)

test_Head :: Suite
test_Head = property "head" $ prop_Text
	(do
		x <- ET.head
		extra <- ET.consume
		return (x, extra)
	)
	(\text -> Right $ case TL.uncons text of
		Nothing -> (Nothing, TL.empty)
		Just (x, extra) -> (Just x, extra))

test_Take :: Suite
test_Take = property "take" $ prop_TextN
	(\n -> do
		xs <- ET.take n
		extra <- ET.consume
		return (xs, extra))
	(\n -> Right . TL.splitAt (fromInteger n))
