{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Iterate
	( test_Iterate
	, test_IterateM
	) where

import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

test_Iterate :: Suite
test_Iterate = assertions "iterate" $ do
	$expect $ equal
		["A", "B", "C"]
		(runIdentity (E.run_ (EB.iterate succ 0x41 $$ EL.take 3)))

test_IterateM :: Suite
test_IterateM = assertions "iterateM" $ do
	let succM = return . succ
	$expect $ equal
		["A", "B", "C"]
		(runIdentity (E.run_ (EB.iterateM succM 0x41 $$ EL.take 3)))
