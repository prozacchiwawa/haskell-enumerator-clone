{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Text.Handle
	( test_EnumHandle
	, test_IterHandle
	, test_EnumFile
	) where

import           Test.Chell

#ifdef MIN_VERSION_knob
import           Data.Knob
#endif

import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified System.IO as IO

import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET

import           Paths_enumerator_tests (getDataFileName)

#ifdef MIN_VERSION_knob

test_EnumHandle :: Suite
test_EnumHandle = assertions "enumHandle" $ do
	do
		knob <- newKnob "0123\n\n4567"
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (ET.enumHandle h $$ EL.consume)
		$expect (equal chunks ["0123\n", "\n", "4567"])
	do
		knob <- newKnob "0123\r\n\n4567\r\n"
		chunks <- withFileHandle knob "" IO.ReadMode $ \h -> do
			E.run_ (ET.enumHandle h $$ EL.consume)
		$expect (equal chunks ["0123\r\n", "\n", "4567\r\n"])

test_IterHandle :: Suite
test_IterHandle = assertions "iterHandle" $ do
	knob <- newKnob ""
	withFileHandle knob "" IO.WriteMode $ \h -> do
		E.run_ (E.enumLists [[], ["A", "B"], ["C"]] $$ ET.iterHandle h)
	bytes <- Data.Knob.getContents knob
	$expect (equal bytes "ABC")

#else

import           EnumeratorTests.Util (todo)

test_EnumHandle :: Suite
test_EnumHandle = todo "enumHandle"

test_IterHandle :: Suite
test_IterHandle = todo "iterHandle"

#endif

lines_LF :: [Text]
lines_LF =
	[ "hello world\n"
	, "\n"
	, "\20320\22909\19990\30028\n"
	, "\n"
	, "\1605\1585\1581\1576\1575 \1575\1604\1593\1575\1604\1605\n"
	, "\n"
	, "\12371\12435\12395\12385\12399\19990\30028\n"
	, "\n"
	, "\2997\2979\2965\3021\2965\2990\3021\n"
	]

lines_CRLF :: [Text]
lines_CRLF =
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

-- define locally, because it's not present in GHC 6.10
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

test_EnumFile :: Suite
test_EnumFile = assertions "enumFile" $ do
	do
		path <- liftIO (getDataFileName "data/utf8-lf.txt")
		chunks <- liftIO (E.run_ (ET.enumFile path $$ EL.consume))
		$expect (equal chunks lines_LF)
	do
		path <- liftIO (getDataFileName "data/utf8-crlf.txt")
		chunks <- liftIO (E.run_ (ET.enumFile path $$ EL.consume))
		$expect (equal chunks (case nativeNewline of
			LF -> lines_CRLF
			CRLF -> lines_LF))
