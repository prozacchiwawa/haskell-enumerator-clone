{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Require
	( test_Require
	) where

import qualified Control.Exception as Exc
import qualified Data.ByteString.Lazy as BL

import           Test.Chell
import           Test.Chell.QuickCheck

import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Binary.Util

test_Require :: Suite
test_Require = property "require" $ prop_BytesN
	(\n -> do
		EB.require n
		EB.consume)
	(\n xs -> if n > toInteger (BL.length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)
