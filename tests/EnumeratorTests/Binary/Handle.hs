{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Handle
	( test_EnumHandle
	, test_EnumHandleRange
	, test_IterHandle
	) where

import           Test.Chell

#ifdef MIN_VERSION_knob

import           Data.Knob
import qualified System.IO as IO

import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

test_EnumHandle :: Suite
test_EnumHandle = assertions "enumHandle" $ do
	knob <- newKnob "01234567"
	chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
		E.run_ (EB.enumHandle 3 h $$ EL.consume)
	$expect (equal chunks ["012", "345", "67"])

test_EnumHandleRange :: Suite
test_EnumHandleRange = assertions "enumHandleRange" $ do
	knob <- newKnob "01234567"
	
	-- no offset or count
	do
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (EB.enumHandleRange 3 Nothing Nothing h $$ EL.consume)
		$expect (equal chunks ["012", "345", "67"])
	
	-- offset
	do
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (EB.enumHandleRange 3 (Just 1) Nothing h $$ EL.consume)
		$expect (equal chunks ["123", "456", "7"])
	
	-- count
	do
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (EB.enumHandleRange 3 Nothing (Just 7) h $$ EL.consume)
		$expect (equal chunks ["012", "345", "6"])
	
	-- count beyond EOF
	do
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (EB.enumHandleRange 3 Nothing (Just 10) h $$ EL.consume)
		$expect (equal chunks ["012", "345", "67"])
	
	-- offset + count
	do
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (EB.enumHandleRange 3 (Just 1) (Just 6) h $$ EL.consume)
		$expect (equal chunks ["123", "456"])

test_IterHandle :: Suite
test_IterHandle = assertions "iterHandle" $ do
	knob <- newKnob ""
	withFileHandle knob "" IO.WriteMode $ \h -> do
		E.run_ (E.enumLists [[], ["A", "B"], ["C"]] $$ EB.iterHandle h)
	bytes <- Data.Knob.getContents knob
	$expect (equal bytes "ABC")

#else

import           EnumeratorTests.Util (todo)

test_EnumHandle :: Suite
test_EnumHandle = todo "enumHandle"

test_EnumHandleRange :: Suite
test_EnumHandleRange = todo "enumHandleRange"

test_IterHandle :: Suite
test_IterHandle = todo "iterHandle"

#endif
