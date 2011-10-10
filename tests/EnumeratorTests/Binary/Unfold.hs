
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Unfold
	( test_Unfold
	, test_UnfoldM
	) where

import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

test_Unfold :: Suite
test_Unfold = assertions "unfold" $ do
	let step x = if x > 0x43
		then Nothing
		else Just (x, succ x)
	$expect $ equal
		["A", "B", "C"]
		(runIdentity (E.run_ (EB.unfold step 0x41 $$ EL.consume)))

test_UnfoldM :: Suite
test_UnfoldM = assertions "unfoldM" $ do
	let step x = return $ if x > 0x43
		then Nothing
		else Just (x, succ x)
	$expect $ equal
		["A", "B", "C"]
		(runIdentity (E.run_ (EB.unfoldM step 0x41 $$ EL.consume)))
