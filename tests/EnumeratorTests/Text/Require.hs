{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Require
	( test_Require
	) where

import qualified Control.Exception as Exc
import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util

test_Require :: Suite
test_Require = property "require" $ prop_TextN
	(\n -> do
		ET.require n
		ET.consume)
	(\n xs -> if n > toInteger (TL.length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)
