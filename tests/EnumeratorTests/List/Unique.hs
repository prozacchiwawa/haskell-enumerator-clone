{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Unique
	( test_Unique
	) where

import           Test.Chell

import           Data.Enumerator ((=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_Unique :: Suite
test_Unique = assertions "unique" $ do
	$expect $ equal
		['B', 'A', 'C']
		(E.runLists_ [['B'], ['A'], ['B'], ['C'], ['A']] $ do
			EL.unique =$ EL.consume)
