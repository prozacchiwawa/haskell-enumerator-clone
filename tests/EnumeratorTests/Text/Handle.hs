{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Handle
	( test_EnumHandle
	, test_IterHandle
	) where

import           Data.Knob
import qualified System.IO as IO
import           Test.Chell

import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

test_EnumHandle :: Suite
test_EnumHandle = assertions "enumHandle" $ do
	knob <- newKnob "0123\n\n4567"
	chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
		E.run_ (ET.enumHandle h $$ EL.consume)
	$expect (equal chunks ["0123", "", "4567"])

test_IterHandle :: Suite
test_IterHandle = assertions "iterHandle" $ do
	knob <- newKnob ""
	withFileHandle knob "" IO.WriteMode $ \h -> do
		E.run_ (E.enumList 1 ["A", "B", "C"] $$ ET.iterHandle h)
	bytes <- Data.Knob.getContents knob
	$expect (equal bytes "ABC")
