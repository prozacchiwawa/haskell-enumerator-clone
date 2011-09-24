{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Isolate
	( test_Isolate
	) where

import qualified Data.Text.Lazy as TL

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util (prop_Text)

test_Isolate :: Suite
test_Isolate = property "isolate" $ prop_Text
	(do
		x <- E.joinI (ET.isolate 2 $$ ET.head)
		extra <- ET.consume
		return (x, extra))
	(\text -> Right $ case TL.unpack text of
		[] -> (Nothing, TL.empty)
		(x:[]) -> (Just x, TL.empty)
		(x:_:xs') -> (Just x, TL.pack xs'))
