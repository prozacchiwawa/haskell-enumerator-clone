{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Fold
	( test_Fold
	, test_FoldM
	) where

import           Control.Monad (foldM)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Text
import           Data.Text (Text)
import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck.Poly
import           Test.QuickCheck.Modifiers

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Text as ET

import           EnumeratorTests.Util ()

test_Fold :: Suite
test_Fold = property "fold" prop_Fold

prop_Fold :: Blind (B -> Char -> B) -> B -> Text -> Bool
prop_Fold (Blind f) z text = result == expected where
	result = E.runLists_ [[text]] (ET.fold f z)
	expected = Data.Text.foldl' f z text

test_FoldM :: Suite
test_FoldM = property "foldM" prop_FoldM

prop_FoldM :: Blind (B -> Char -> B) -> B -> Text -> Bool
prop_FoldM (Blind f) z text = result == expected where
	result = E.runLists_ [[text]] (ET.foldM f' z)
	expected = runIdentity (foldM f' z (Data.Text.unpack text))
	f' b a = return (f b a)
