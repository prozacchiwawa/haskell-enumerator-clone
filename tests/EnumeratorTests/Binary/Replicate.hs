{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Replicate
	( test_Replicate
	, test_ReplicateM
	) where

import           Control.Monad.Trans.State
import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

test_Replicate :: Suite
test_Replicate = assertions "replicate" $ do
	$expect $ equal
		["A", "A", "A"]
		(runIdentity (E.run_ (EB.replicate 3 0x41 $$ EL.consume)))

test_ReplicateM :: Suite
test_ReplicateM = assertions "repeatM" $ do
	let step = do
		c <- get
		put (succ c)
		return c
	$expect $ equal
		["A", "B", "C"]
		(evalState (E.run_ (EB.replicateM 3 step $$ EL.consume)) 0x41)
	$expect $ equal
		["A", "B"]
		(evalState (E.run_ (EB.replicateM 3 step $$ EL.take 2)) 0x41)
