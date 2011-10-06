{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Replicate
	( test_Replicate
	, test_ReplicateM
	) where

import           Control.Monad.Trans.State
import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

test_Replicate :: Suite
test_Replicate = assertions "replicate" $ do
	$expect $ equal
		["A", "A", "A"]
		(runIdentity (E.run_ (ET.replicate 3 'A' $$ EL.consume)))

test_ReplicateM :: Suite
test_ReplicateM = assertions "repeatM" $ do
	let step = do
		c <- get
		put (succ c)
		return c
	$expect $ equal
		["A", "B", "C"]
		(evalState (E.run_ (ET.replicateM 3 step $$ EL.consume)) 'A')
	$expect $ equal
		["A", "B"]
		(evalState (E.run_ (ET.replicateM 3 step $$ EL.take 2)) 'A')
