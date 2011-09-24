{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Drop
	( test_Drop
	, test_Filter
	, test_FilterM
	) where

import qualified Data.ByteString.Lazy as BL

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Util (todo)
import           EnumeratorTests.Binary.Util (prop_BytesN)

test_Drop :: Suite
test_Drop = property "drop" $ prop_BytesN
	(\n -> EB.drop n >> EB.consume)
	(\n -> Right . BL.drop (fromInteger n))

test_Filter :: Suite
test_Filter = todo "filter"

test_FilterM :: Suite
test_FilterM = todo "filterM"
