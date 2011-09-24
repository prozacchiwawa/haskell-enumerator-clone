{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Require
	( test_Require
	) where

import qualified Control.Exception as Exc

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_Require :: Suite
test_Require = property "require" $ prop_ListN
	(\n -> do
		EL.require n
		EL.consume)
	(\n xs -> if n > toInteger (length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)
