{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Util
	( prop_Text
	, prop_TextN
	, prop_TextX
	) where

import           Control.Exception (ErrorCall)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Functor.Identity (Identity)

import           Test.QuickCheck hiding (property)

import           Data.Enumerator (Iteratee)

import           EnumeratorTests.Util (check)

prop_Text :: Eq b
          => Iteratee Text Identity b
          -> (TL.Text -> Either ErrorCall b)
          -> [Text]
          -> Bool
prop_Text iter plain = check iter (plain . TL.fromChunks)

prop_TextN :: Eq b
           => (Integer -> Iteratee Text Identity b)
           -> (Integer -> TL.Text -> Either ErrorCall b)
           -> Positive Integer
           -> [Text]
           -> Bool
prop_TextN iter plain (Positive n) = check (iter n) (plain n . TL.fromChunks)

prop_TextX :: Eq b
           => (Char -> Iteratee Text Identity b)
           -> (Char -> TL.Text -> Either ErrorCall b)
           -> Char
           -> [Text]
           -> Bool
prop_TextX iter plain x = check (iter x) (plain x . TL.fromChunks)
