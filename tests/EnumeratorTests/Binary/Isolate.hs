{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Isolate
	( test_Isolate
	) where

import qualified Data.ByteString.Lazy as BL

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Binary.Util (prop_Bytes)

test_Isolate :: Suite
test_Isolate = property "isolate" $ prop_Bytes
	(do
		x <- E.joinI (EB.isolate 2 $$ EB.head)
		extra <- EB.consume
		return (x, extra))
	(\bytes -> Right $ case BL.unpack bytes of
		[] -> (Nothing, BL.empty)
		(x:[]) -> (Just x, BL.empty)
		(x:_:xs) -> (Just x, BL.pack xs))
