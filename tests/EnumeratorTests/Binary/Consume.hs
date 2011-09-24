{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Consume
	( test_Consume
	, test_Head
	, test_Take
	) where

import qualified Data.ByteString.Lazy as BL

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Binary.Util

test_Consume :: Suite
test_Consume = property "consume" (prop_Bytes EB.consume Right)

test_Head :: Suite
test_Head = property "head" $ prop_Bytes
	(do
		x <- EB.head
		extra <- EB.consume
		return (x, extra)
	)
	(\text -> Right $ case BL.uncons text of
		Nothing -> (Nothing, BL.empty)
		Just (x, extra) -> (Just x, extra))

test_Take :: Suite
test_Take = property "take" $ prop_BytesN
	(\n -> do
		xs <- EB.take n
		extra <- EB.consume
		return (xs, extra))
	(\n -> Right . BL.splitAt (fromInteger n))
