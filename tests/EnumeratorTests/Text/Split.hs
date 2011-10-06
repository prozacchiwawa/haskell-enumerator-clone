{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Split
	( test_SplitWhen
	, test_Lines
	) where

import           Data.Functor.Identity (runIdentity)
import qualified Data.List.Split as LS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Test.Chell
import           Test.Chell.QuickCheck

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Text.Util

test_SplitWhen :: Suite
test_SplitWhen = property "splitWhen" $ prop_TextX
	(\c -> do
		xs <- E.joinI (ET.splitWhen (== c) $$ EL.consume)
		extra <- EL.consume
		return (xs, extra))
	(\c text -> let
		split = LS.split . LS.dropFinalBlank . LS.dropDelims . LS.whenElt
		chars = TL.unpack text
		in Right (map T.pack (split (== c) chars), []))

test_Lines :: Suite
test_Lines = assertions "lines" $ do
	$expect $ equal
		["abc", "def"]
		(runIdentity (E.run_ (E.enumList 1 ["abc\ndef"] $$ E.joinI (ET.lines $$ EL.consume))))
	$expect $ equal
		["abc", "def"]
		(runIdentity (E.run_ (E.enumList 1 ["abc\ndef\n"] $$ E.joinI (ET.lines $$ EL.consume))))
	$expect $ equal
		["abc", "def", ""]
		(runIdentity (E.run_ (E.enumList 1 ["abc\ndef\n\n"] $$ E.joinI (ET.lines $$ EL.consume))))
