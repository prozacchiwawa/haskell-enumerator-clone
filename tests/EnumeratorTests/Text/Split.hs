{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Split
	( test_SplitWhen
	) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.List.Split as LS
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
