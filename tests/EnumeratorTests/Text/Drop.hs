{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Drop
	( test_Drop
	, test_Filter
	, test_FilterM
	) where

import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Util (todo)
import           EnumeratorTests.Text.Util (prop_TextN)

test_Drop :: Suite
test_Drop = property "drop" $ prop_TextN
	(\n -> ET.drop n >> ET.consume)
	(\n -> Right . TL.drop (fromInteger n))

test_Filter :: Suite
test_Filter = todo "filter"

test_FilterM :: Suite
test_FilterM = todo "filterM"
