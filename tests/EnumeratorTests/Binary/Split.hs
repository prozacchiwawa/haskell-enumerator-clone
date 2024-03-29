{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Split
	( test_SplitWhen
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.List.Split as LS
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Binary.Util

test_SplitWhen :: Suite
test_SplitWhen = suite "splitWhen"
	[ prop_SplitWhen
	, test_HandleEmpty
	]

prop_SplitWhen :: Suite
prop_SplitWhen = property "model" $ prop_BytesX
	(\c -> do
		xs <- E.joinI (EB.splitWhen (== c) $$ EL.consume)
		extra <- EL.consume
		return (xs, extra))
	(\c text -> let
		split = LS.split . LS.dropFinalBlank . LS.dropDelims . LS.whenElt
		chars = BL.unpack text
		in Right (map B.pack (split (== c) chars), []))

test_HandleEmpty :: Suite
test_HandleEmpty = assertions "empty" $ do
	$expect $ equal
		([], Nothing)
		(E.runLists_ [[""]] $ do
			xs <- EB.splitWhen (== 0x2C) =$ EL.consume
			extra <- EL.head
			return (xs, extra))
