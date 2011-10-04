{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Codecs
	( test_TextCodecs
	) where

import           Prelude hiding (words)

import           Control.Exception (ErrorCall(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util

test_TextCodecs :: Suite
test_TextCodecs = suite "codecs"
	[ test_ASCII
	, test_ISO8859_1
	, test_UTF8
	, test_UTF16_BE
	, test_UTF16_LE
	, test_UTF32_BE
	, test_UTF32_LE
	]

test_ASCII :: Suite
test_ASCII = suite "ascii"
	[ property "encode" (forAll genASCII (prop_Encode ET.ascii encodeASCII))
	, property "decode" (forAll genASCII (prop_Decode ET.ascii decodeASCII))
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"ASCII\""
	  		(show ET.ascii)
	
	, assertions "encode-invalid" $ do
	  	$expect $ throwsEq
	  		(ErrorCall "Codec \"ASCII\" can't encode character U+00FF")
	  		(runList ["\xFF"] (E.joinI (ET.encode ET.ascii $$ EB.consume)))
	
	, assertions "decode-invalid" $ do
	  	$expect $ throwsEq
	  		(ErrorCall "Codec \"ASCII\" can't decode byte 0xFF")
	  		(runList ["\xFF"] (E.joinI (ET.decode ET.ascii $$ ET.consume)))
	
	, assertions "lazy" $ do
	  	{-
	  	$expect $ equal
	  		(Just 0x61, ["b"])
	  		(runListI ["", "ab"] (do
	  			x <- E.joinI (ET.encode ET.ascii $$ EB.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	-}
	  	$expect $ equal
	  		(Just 0x61, ["b"])
	  		(runListI ["", "a", "b"] (do
	  			x <- E.joinI (ET.encode ET.ascii $$ EB.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 0x61, ["\xFF"])
	  		(runListI ["", "a\xFF"] (do
	  			x <- E.joinI (ET.encode ET.ascii $$ EB.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["b"])
	  		(runListI ["", "a", "b"] (do
	  			x <- E.joinI (ET.decode ET.ascii $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["\xFF"])
	  		(runListI ["", "a\xFF"] (do
	  			x <- E.joinI (ET.decode ET.ascii $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

encodeASCII :: T.Text -> B.ByteString
encodeASCII text = if T.any (\c -> ord c > 127) text
	then error "encodeASCII: input contains non-ASCII characters."
	else B8.pack (T.unpack text)

decodeASCII :: B.ByteString -> T.Text
decodeASCII bytes = if B.any (> 127) bytes
	then error "decodeASCII: input contains non-ASCII characters."
	else T.pack (B8.unpack bytes)

test_ISO8859_1 :: Suite
test_ISO8859_1 = suite "iso8859-1"
	[ property "encode" (forAll genISO8859_1 (prop_Encode ET.iso8859_1 encodeISO8859_1))
	, property "decode" (forAll genISO8859_1 (prop_Decode ET.iso8859_1 decodeISO8859_1))
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"ISO-8859-1\""
	  		(show ET.iso8859_1)
	
	, assertions "encode-invalid" $ do
	  	$expect $ throwsEq
	  		(ErrorCall "Codec \"ISO-8859-1\" can't encode character U+01FF")
	  		(runList ["\x1FF"] (E.joinI (ET.encode ET.iso8859_1 $$ EB.consume)))
	
	, assertions "lazy" $ do
	  	$expect $ equal
	  		(Just 0x61, ["b"])
	  		(runListI ["", "a", "b"] (do
	  			x <- E.joinI (ET.encode ET.iso8859_1 $$ EB.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 0x61, ["\x1FF"])
	  		(runListI ["", "a\x1FF"] (do
	  			x <- E.joinI (ET.encode ET.iso8859_1 $$ EB.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["b"])
	  		(runListI ["", "a", "b"] (do
	  			x <- E.joinI (ET.decode ET.iso8859_1 $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

encodeISO8859_1 :: T.Text -> B.ByteString
encodeISO8859_1 text = if T.any (\c -> ord c > 255) text
	then error "encodeASCII: input contains non-ISO8859-1 characters."
	else B8.pack (T.unpack text)

decodeISO8859_1 :: B.ByteString -> T.Text
decodeISO8859_1 bytes = T.pack (B8.unpack bytes)

test_UTF8 :: Suite
test_UTF8 = suite "utf8"
	[ property "encode" (prop_Encode ET.utf8 TE.encodeUtf8)
	, property "decode" (prop_Decode ET.utf8 TE.decodeUtf8 . TE.encodeUtf8)
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"UTF-8\""
	  		(show ET.utf8)
	
	, assertions "decode-invalid" $ do
	  	$expect $ throwsEq
	  		(TE.DecodeError "Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream" (Just 0xFF))
	  		(runList ["\xFF"] (E.joinI (ET.decode ET.utf8 $$ ET.consume)))
	  	$expect $ throwsEq
	  		(ErrorCall "Unexpected EOF while decoding")
	  		(runList ["\xF0"] (E.joinI (ET.decode ET.utf8 $$ ET.consume)))
	
	, assertions "lazy" $ do
	  	$expect $ equal
	  		(Just 'a', ["\xEF\xBD"])
	  		(runListI ["a\xEF\xBD"] (do
	  			x <- E.joinI (ET.decode ET.utf8 $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

test_UTF16_BE :: Suite
test_UTF16_BE = suite "utf16-be"
	[ property "encode" (prop_Encode ET.utf16_be TE.encodeUtf16BE)
	, property "decode" (prop_Decode ET.utf16_be TE.decodeUtf16BE . TE.encodeUtf16BE)
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"UTF-16-BE\""
	  		(show ET.utf16_be)
	
	, assertions "decode-invalid" $ do
	  	$expect $ throwsEq
	  		(TE.DecodeError "Data.Text.Encoding.Fusion.streamUtf16BE: Invalid UTF-16BE stream" Nothing)
	  		(runList ["\xDD\x1E"] (E.joinI (ET.decode ET.utf16_be $$ ET.consume)))
	  	$expect $ throwsEq
	  		(ErrorCall "Unexpected EOF while decoding")
	  		(runList ["\xD8\x00"] (E.joinI (ET.decode ET.utf16_be $$ ET.consume)))
	
	, assertions "lazy" $ do
	  	$expect $ equal
	  		(Just 'a', ["\x00"])
	  		(runListI ["\x00\x61\x00"] (do
	  			x <- E.joinI (ET.decode ET.utf16_be $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["\xDD\x1E"])
	  		(runListI ["\x00\x61\xDD\x1E"] (do
	  			x <- E.joinI (ET.decode ET.utf16_be $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

test_UTF16_LE :: Suite
test_UTF16_LE = suite "utf16-le"
	[ property "encode" (prop_Encode ET.utf16_le TE.encodeUtf16LE)
	, property "decode" (prop_Decode ET.utf16_le TE.decodeUtf16LE . TE.encodeUtf16LE)
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"UTF-16-LE\""
	  		(show ET.utf16_le)
	
	, assertions "decode-invalid" $ do
	  	$expect $ throwsEq
	  		(TE.DecodeError "Data.Text.Encoding.Fusion.streamUtf16LE: Invalid UTF-16LE stream" Nothing)
	  		(runList ["\x1E\xDD"] (E.joinI (ET.decode ET.utf16_le $$ ET.consume)))
	  	$expect $ throwsEq
	  		(ErrorCall "Unexpected EOF while decoding")
	  		(runList ["\x00\xD8"] (E.joinI (ET.decode ET.utf16_le $$ ET.consume)))
	
	, assertions "lazy" $ do
	  	$expect $ equal
	  		(Just 'a', ["\x00"])
	  		(runListI ["\x61\x00\x00"] (do
	  			x <- E.joinI (ET.decode ET.utf16_le $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["\x1E\xDD"])
	  		(runListI ["\x61\x00\x1E\xDD"] (do
	  			x <- E.joinI (ET.decode ET.utf16_le $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

test_UTF32_BE :: Suite
test_UTF32_BE = suite "utf32-be"
	[ property "encode" (prop_Encode ET.utf32_be TE.encodeUtf32BE)
	, property "decode" (prop_Decode ET.utf32_be TE.decodeUtf32BE . TE.encodeUtf32BE)
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"UTF-32-BE\""
	  		(show ET.utf32_be)
	
	, assertions "decode-invalid" $ do
	  	$expect $ throwsEq
	  		(TE.DecodeError "Data.Text.Encoding.Fusion.streamUtf32BE: Invalid UTF-32BE stream" Nothing)
	  		(runList ["\xFF\xFF\xFF\xFF"] (E.joinI (ET.decode ET.utf32_be $$ ET.consume)))
	  	$expect $ throwsEq
	  		(ErrorCall "Unexpected EOF while decoding")
	  		(runList ["\x00\x00"] (E.joinI (ET.decode ET.utf32_be $$ ET.consume)))
	
	, assertions "lazy" $ do
	  	$expect $ equal
	  		(Just 'a', ["\x00"])
	  		(runListI ["\x00\x00\x00\x61\x00"] (do
	  			x <- E.joinI (ET.decode ET.utf32_be $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["\xFF\xFF\xFF\xFF"])
	  		(runListI ["\x00\x00\x00\x61\xFF\xFF\xFF\xFF"] (do
	  			x <- E.joinI (ET.decode ET.utf32_be $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

test_UTF32_LE :: Suite
test_UTF32_LE = suite "utf32-le"
	[ property "encode" (prop_Encode ET.utf32_le TE.encodeUtf32LE)
	, property "decode" (prop_Decode ET.utf32_le TE.decodeUtf32LE . TE.encodeUtf32LE)
	
	, assertions "show" $ do
	  	$expect $ equal
	  		"Codec \"UTF-32-LE\""
	  		(show ET.utf32_le)
	
	, assertions "decode-invalid" $ do
	  	$expect $ throwsEq
	  		(TE.DecodeError "Data.Text.Encoding.Fusion.streamUtf32LE: Invalid UTF-32LE stream" Nothing)
	  		(runList ["\xFF\xFF\xFF\xFF"] (E.joinI (ET.decode ET.utf32_le $$ ET.consume)))
	  	$expect $ throwsEq
	  		(ErrorCall "Unexpected EOF while decoding")
	  		(runList ["\x00\x00"] (E.joinI (ET.decode ET.utf32_le $$ ET.consume)))
	
	, assertions "lazy" $ do
	  	$expect $ equal
	  		(Just 'a', ["\x00"])
	  		(runListI ["\x61\x00\x00\x00\x00"] (do
	  			x <- E.joinI (ET.decode ET.utf32_le $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	  	$expect $ equal
	  		(Just 'a', ["\xFF\xFF\xFF\xFF"])
	  		(runListI ["\x61\x00\x00\x00\xFF\xFF\xFF\xFF"] (do
	  			x <- E.joinI (ET.decode ET.utf32_le $$ ET.head)
	  			y <- EL.consume
	  			return (x, y)))
	]

prop_Encode :: ET.Codec -> (T.Text -> B.ByteString) -> T.Text -> Bool
prop_Encode codec model text = encoded == model text where
	lazy = runListI [text] (E.joinI (ET.encode codec $$ EB.consume))
	encoded = B.concat (BL.toChunks lazy)

prop_Decode :: ET.Codec -> (B.ByteString -> T.Text) -> B.ByteString -> Bool
prop_Decode codec model bytes = decoded == model bytes where
	lazy = runListI [bytes] (E.joinI (ET.decode codec $$ ET.consume))
	decoded = TL.toStrict lazy

runList :: Monad m => [a] -> E.Iteratee a m b -> m b
runList xs iter = E.run_ (E.enumList 1 xs $$ iter)

runListI :: [a] -> E.Iteratee a Identity b -> b
runListI xs iter = runIdentity (runList xs iter)
