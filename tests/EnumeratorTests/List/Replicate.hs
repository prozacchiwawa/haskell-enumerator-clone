{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Replicate
	( test_Replicate
	, test_ReplicateM
	) where

import           Control.Monad.Trans.State
import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

test_Replicate :: Suite
test_Replicate = assertions "replicate" $ do
	$expect $ equal
		['A', 'A', 'A']
		(runIdentity (E.run_ (EL.replicate 3 'A' $$ EL.consume)))

test_ReplicateM :: Suite
test_ReplicateM = assertions "replicateM" $ do
	let step = do
		c <- get
		put (succ c)
		return c
	$expect $ equal
		['A', 'B', 'C']
		(evalState (E.run_ (EL.replicateM 3 step $$ EL.consume)) 'A')
	$expect $ equal
		['A', 'B']
		(evalState (E.run_ (EL.replicateM 3 step $$ EL.take 2)) 'A')
