{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Codecs
	( test_TextCodecs
	) where

import           Prelude hiding (words)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util

test_TextCodecs :: Suite
test_TextCodecs = suite "codecs"
	[ test_ASCII
	, test_ISO8859_1
	, test_UTF8
	, test_UTF16
	, test_UTF32
	]

test_ASCII :: Suite
test_ASCII = suite "ascii"
	[ test_Encode_ASCII
	, test_Decode_ASCII
	]

test_ISO8859_1 :: Suite
test_ISO8859_1 = suite "iso8859-1"
	[ test_Encode_ISO8859_1
	, test_Decode_ISO8859_1
	]

test_UTF8 :: Suite
test_UTF8 = suite "utf8"
	[ test_Encode_UTF8
	, test_Decode_UTF8
	]

test_UTF16 :: Suite
test_UTF16 = suite "utf16"
	[ test_Encode_UTF16_BE
	, test_Encode_UTF16_LE
	, test_Decode_UTF16_BE
	, test_Decode_UTF16_LE
	]

test_UTF32 :: Suite
test_UTF32 = suite "utf32"
	[ test_Encode_UTF32_BE
	, test_Encode_UTF32_LE
	, test_Decode_UTF32_BE
	, test_Decode_UTF32_LE
	]

test_Encode_ASCII :: Suite
test_Encode_ASCII = suite "encode" props where
	props = [ property "works" (forAll genASCII prop_works)
	        , property "error" prop_error
	        , property "lazy" prop_lazy
	        ]
	
	encode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.encode ET.ascii $$ iter)
	
	prop_works bytes = result == map B.singleton words where
		Right result = encode EL.consume (map T.singleton chars)
		
		chars = B8.unpack bytes
		words = B.unpack bytes
	
	prop_error = isLeft (encode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [T.pack "\x61\xFF"]
	
	prop_lazy = either (const False) (== expected) result where
		result = encode EL.head input
		input = [T.pack "\x61\xFF"]
		expected = Just (B.singleton 0x61)

test_Decode_ASCII :: Suite
test_Decode_ASCII = suite "decode" props where
	props = [ property "works" (forAll genASCII prop_works)
	        , property "error" prop_error
	        , property "lazy" prop_lazy
	        ]
	
	decode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.decode ET.ascii $$ iter)
	
	prop_works text = result == map T.singleton chars where
		Right result = decode EL.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf8 text)
		chars = T.unpack text
	
	prop_error = isLeft (decode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0xFF]]
	
	prop_lazy = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x61, 0xFF]]
		expected = Just (T.pack "a")

test_Encode_ISO8859_1 :: Suite
test_Encode_ISO8859_1 = suite "encode" props where
	props = [ property "works" (forAll genISO8859_1 prop_works)
	        , property "error" prop_error
	        , property "lazy" prop_lazy
	        ]
	
	encode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.encode ET.iso8859_1 $$ iter)
	
	prop_works bytes = result == map B.singleton words where
		Right result = encode EL.consume (map T.singleton chars)
		
		chars = B8.unpack bytes
		words = B.unpack bytes
	
	prop_error = isLeft (encode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [T.pack "\x61\xFF5E"]
	
	prop_lazy = either (const False) (== expected) result where
		result = encode EL.head input
		input = [T.pack "\x61\xFF5E"]
		expected = Just (B.singleton 0x61)

test_Decode_ISO8859_1 :: Suite
test_Decode_ISO8859_1 = todo "decode"

test_Encode_UTF8 :: Suite
test_Encode_UTF8 = todo "encode"

test_Decode_UTF8 :: Suite
test_Decode_UTF8 = suite "decode" props where
	props = [ property "works" prop_works
	        , property "error" prop_error
	        , property "lazy" prop_lazy
	        , property "incremental" prop_incremental
	        ]
	
	decode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.decode ET.utf8 $$ iter)
	
	prop_works text = result == map T.singleton chars where
		Right result = decode EL.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf8 text)
		chars = T.unpack text
	
	prop_error = isLeft (decode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0x61, 0x80]]
	
	prop_lazy = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x61, 0x80]]
		expected = Just (T.pack "a")
	
	prop_incremental = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x61, 0xC2, 0xC2]]
		expected = Just (T.pack "a")

test_Encode_UTF16_BE :: Suite
test_Encode_UTF16_BE = todo "encode"

test_Decode_UTF16_BE :: Suite
test_Decode_UTF16_BE = suite "decode" props where
	props = [ property "works" prop_works
	        , property "lazy" prop_lazy
	        , property "error" prop_error
	        , property "incremental" prop_incremental
	        ]
	
	decode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.decode ET.utf16_be $$ iter)
	
	prop_works text = result == map T.singleton chars where
		Right result = decode EL.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf16BE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x00, 0x61, 0xDD, 0x1E]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0x00, 0x61, 0xDD, 0x1E]]
	
	prop_incremental = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x00, 0x61, 0xD8, 0x34, 0xD8, 0xD8]]
		expected = Just (T.pack "a")

test_Encode_UTF16_LE :: Suite
test_Encode_UTF16_LE = todo "encode"

test_Decode_UTF16_LE :: Suite
test_Decode_UTF16_LE = suite "decode" props where
	props = [ property "works" prop_works
	        , property "lazy" prop_lazy
	        , property "error" prop_error
	        , property "incremental" prop_incremental
	        ]
	
	decode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.decode ET.utf16_le $$ iter)
	
	prop_works text = result == map T.singleton chars where
		Right result = decode EL.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf16LE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x61, 0x00, 0x1E, 0xDD]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0x61, 0x00, 0x1E, 0xDD]]
	
	prop_incremental = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x61, 0x00, 0x34, 0xD8, 0xD8, 0xD8]]
		expected = Just (T.pack "a")

test_Encode_UTF32_BE :: Suite
test_Encode_UTF32_BE = todo "encode"

test_Decode_UTF32_BE :: Suite
test_Decode_UTF32_BE = suite "decode" props where
	props = [ property "works" prop_works
	        , property "lazy" prop_lazy
	        , property "error" prop_error
	        ]
	
	decode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.decode ET.utf32_be $$ iter)
	
	prop_works text = result == map T.singleton chars where
		Right result = decode EL.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf32BE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x00, 0x00, 0x00, 0x61, 0xFF, 0xFF]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0xFF, 0xFF, 0xFF, 0xFF]]

test_Encode_UTF32_LE :: Suite
test_Encode_UTF32_LE = todo "encode"

test_Decode_UTF32_LE :: Suite
test_Decode_UTF32_LE = suite "decode" props where
	props = [ property "works" prop_works
	        , property "lazy" prop_lazy
	        , property "error" prop_error
	        ]
	
	decode iter input =
		runIdentity . E.run $
		E.enumList 1 input $$
		E.joinI (ET.decode ET.utf32_le $$ iter)
	
	prop_works text = result == map T.singleton chars where
		Right result = decode EL.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf32LE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode EL.head input
		input = [B.pack [0x61, 0x00, 0x00, 0x00, 0xFF, 0xFF]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode EL.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0xFF, 0xFF, 0xFF, 0xFF]]
