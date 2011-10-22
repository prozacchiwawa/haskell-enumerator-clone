{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Split
	( test_SplitWhen
	) where


import qualified Data.List.Split as LS
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator ((=$))
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util

test_SplitWhen :: Suite
test_SplitWhen = property "splitWhen" $ prop_ListX
	(\x -> do
		xs <- EL.splitWhen (== x) =$ EL.consume
		extra <- EL.consume
		return (xs, extra))
	(\x xs -> let
		split = LS.split . LS.dropFinalBlank . LS.dropDelims . LS.whenElt
		in Right (split (== x) xs, []))
