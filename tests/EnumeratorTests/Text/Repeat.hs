{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Repeat
	( test_Repeat
	, test_RepeatM
	, test_GenerateM
	) where

import           Control.Monad.Trans.State
import           Data.Functor.Identity (runIdentity)
import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

test_Repeat :: Suite
test_Repeat = assertions "repeat" $ do
	$expect $ equal
		["A", "A", "A"]
		(runIdentity (E.run_ (ET.repeat 'A' $$ EL.take 3)))

test_RepeatM :: Suite
test_RepeatM = assertions "repeatM" $ do
	let step = do
		c <- get
		put (succ c)
		return c
	$expect $ equal
		["A", "B", "C"]
		(evalState (E.run_ (ET.repeatM step $$ EL.take 3)) 'A')

test_GenerateM :: Suite
test_GenerateM = assertions "generateM" $ do
	let step = do
		c <- get
		if c > 'C'
			then return Nothing
			else do
				put (succ c)
				return (Just c)
	$expect $ equal
		["A", "B", "C"]
		(evalState (E.run_ (ET.generateM step $$ EL.consume)) 'A')
