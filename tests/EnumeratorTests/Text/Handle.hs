{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Handle
	( test_EnumHandle
	, test_EnumFile
	, test_IterHandle
	) where

import           Test.Chell

#ifdef MIN_VERSION_knob
import           Data.Knob
#else
import           EnumeratorTests.Util (todo)
#endif

import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text, replace)
import qualified System.IO as IO

import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           Paths_enumerator_tests (getDataFileName)

test_EnumHandle :: Suite
test_EnumHandle = suite "enumHandle"
	[ test_EnumHandle_AsciiLF
	, test_EnumHandle_AsciiCRLF
	, test_EnumHandle_Utf8LF
	, test_EnumHandle_Utf8CRLF
	]

--- define locally, because it's not present in GHC 6.10
data Newline = LF | CRLF
	deriving (Show, Eq)

nativeNewline :: Newline
#if MIN_VERSION_base(4,2,0)
nativeNewline = case IO.nativeNewline of
	IO.LF -> LF
	IO.CRLF -> CRLF
#else
#ifdef CABAL_OS_WINDOWS
nativeNewline = CRLF
#else
nativeNewline = LF
#endif
#endif

runEnumHandle :: String -> Assertions [Text]
runEnumHandle name = do
	path <- liftIO (getDataFileName name)
	chunks <- liftIO (IO.withFile path IO.ReadMode (\h -> do
#if MIN_VERSION_base(4,2,0)
		IO.hSetEncoding h IO.utf8
#endif
		E.run_ (ET.enumHandle h $$ EL.consume)))
	return chunks

test_EnumHandle_AsciiLF :: Suite
test_EnumHandle_AsciiLF = assertions "ascii-lf" $ do
	chunks <- runEnumHandle "data/ascii-lf.txt"
	$expect (equalItems
		[ "hello\n"
		, "\n"
		, "world\n"
		] chunks)

test_EnumHandle_AsciiCRLF :: Suite
test_EnumHandle_AsciiCRLF = assertions "ascii-crlf" $ do
	chunks <- runEnumHandle "data/ascii-crlf.txt"
	let rawLines =
		[ "hello\r\n"
		, "\r\n"
		, "world\r\n"
		]
	let expected = case nativeNewline of
		CRLF -> map (replace "\r\n" "\n") rawLines
		LF -> rawLines
	$expect (equalItems expected chunks)

test_EnumHandle_Utf8LF :: Suite
test_EnumHandle_Utf8LF = assertions "utf8-lf" $ do
	chunks <- runEnumHandle "data/utf8-lf.txt"
	$expect (equalItems
		[ "hello world\n"
		, "\n"
		, "\20320\22909\19990\30028\n"
		, "\n"
		, "\1605\1585\1581\1576\1575 \1575\1604\1593\1575\1604\1605\n"
		, "\n"
		, "\12371\12435\12395\12385\12399\19990\30028\n"
		, "\n"
		, "\2997\2979\2965\3021\2965\2990\3021\n"
		] chunks)

test_EnumHandle_Utf8CRLF :: Suite
test_EnumHandle_Utf8CRLF = assertions "utf8-crlf" $ do
	chunks <- runEnumHandle "data/utf8-crlf.txt"
	let rawLines =
		[ "hello world\r\n"
		, "\r\n"
		, "\20320\22909\19990\30028\r\n"
		, "\r\n"
		, "\1605\1585\1581\1576\1575 \1575\1604\1593\1575\1604\1605\r\n"
		, "\r\n"
		, "\12371\12435\12395\12385\12399\19990\30028\r\n"
		, "\r\n"
		, "\2997\2979\2965\3021\2965\2990\3021\r\n"
		]
	let expected = case nativeNewline of
		CRLF -> map (replace "\r\n" "\n") rawLines
		LF -> rawLines
	$expect (equalItems expected chunks)

test_EnumFile :: Suite
test_EnumFile = assertions "enumFile" $ do
	path <- liftIO (getDataFileName "data/ascii-lf.txt")
	chunks <- liftIO (E.run_ (ET.enumFile path $$ EL.consume))
	$expect (equal
		[ "hello\n"
		, "\n"
		, "world\n"
		] chunks)

#ifdef MIN_VERSION_knob

test_IterHandle :: Suite
test_IterHandle = assertions "iterHandle" $ do
	knob <- newKnob ""
	withFileHandle knob "" IO.WriteMode $ \h -> do
		E.run_ (E.enumLists [[], ["A", "B"], ["C"]] $$ ET.iterHandle h)
	bytes <- Data.Knob.getContents knob
	$expect (equal bytes "ABC")

#else

test_IterHandle :: Suite
test_IterHandle = todo "iterHandle"

#endif
