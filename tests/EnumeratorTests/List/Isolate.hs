{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Isolate
	( test_Isolate
	) where

import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_Isolate :: Suite
test_Isolate = property "isolate" $ prop_List
	(do
		x <- E.joinI (EL.isolate 2 $$ EL.head)
		extra <- EL.consume
		return (x, extra))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:[]) -> (Just x, [])
		(x:_:xs') -> (Just x, xs'))
