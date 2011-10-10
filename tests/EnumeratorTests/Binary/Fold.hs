{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Fold
	( test_Fold
	, test_FoldM
	) where

import           Control.Monad (foldM)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import           Data.Functor.Identity (runIdentity)
import           Data.Word (Word8)
import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck.Poly
import           Test.QuickCheck.Modifiers

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

import           EnumeratorTests.Util ()

test_Fold :: Suite
test_Fold = property "fold" prop_Fold

prop_Fold :: Blind (B -> Word8 -> B) -> B -> ByteString -> Bool
prop_Fold (Blind f) z text = result == expected where
	result = runIdentity (E.run_ (E.enumList 1 [text] $$ EB.fold f z))
	expected = Data.ByteString.foldl' f z text

test_FoldM :: Suite
test_FoldM = property "foldM" prop_FoldM

prop_FoldM :: Blind (B -> Word8 -> B) -> B -> ByteString -> Bool
prop_FoldM (Blind f) z text = result == expected where
	result = runIdentity (E.run_ (E.enumList 1 [text] $$ EB.foldM f' z))
	expected = runIdentity (foldM f' z (Data.ByteString.unpack text))
	f' b a = return (f b a)
