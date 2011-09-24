{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Binary.Util
	( prop_Bytes
	, prop_BytesN
	, prop_BytesX
	) where

import           Control.Exception (ErrorCall)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (Identity)

import           Test.QuickCheck hiding (property)

import           Data.Enumerator (Iteratee)

import           EnumeratorTests.Util (check)

prop_Bytes :: Eq b
           => Iteratee ByteString Identity b
           -> (BL.ByteString -> Either ErrorCall b)
           -> [ByteString]
           -> Bool
prop_Bytes iter plain = check iter (plain . BL.fromChunks)

prop_BytesN :: Eq b
            => (t -> Iteratee ByteString Identity b)
            -> (t -> BL.ByteString -> Either ErrorCall b)
            -> Positive t
            -> [ByteString]
            -> Bool
prop_BytesN iter plain (Positive n) = check (iter n) (plain n . BL.fromChunks)

prop_BytesX :: Eq b
            => (t -> Iteratee ByteString Identity b)
            -> (t -> BL.ByteString -> Either ErrorCall b)
            -> t
            -> [ByteString]
            -> Bool
prop_BytesX iter plain x = check (iter x) (plain x . BL.fromChunks)
