{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import           Control.Concurrent
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class (liftIO)
import           Data.Bits ((.&.))
import           Data.Char (chr)
import qualified Data.List as L
import qualified Data.List.Split as LS
import           Data.Monoid (mappend, mempty, mconcat)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.String (IsString, fromString)
import           Data.Word (Word8)

import           Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding ((.&.), property)
import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Test.QuickCheck.Poly (A, B, C)

tests :: [Suite]
tests =
	[ suite_StreamInstances
	, suite_Text
	, suite_ListAnalogues
	, suite_Other
	]

main :: IO ()
main = Test.Chell.defaultMain tests

-- Stream instances {{{

suite_StreamInstances :: Suite
suite_StreamInstances = suite "stream-instances"
	[ suite_StreamMonoid
	, suite_StreamFunctor
	, suite_StreamMonad
	]

suite_StreamMonoid :: Suite
suite_StreamMonoid = suite "monoid" props where
	props = [ property "law-1" prop_law1
	        , property "law-2" prop_law2
	        , property "law-3" prop_law3
	        , property "law-4" prop_law4
	        ]
	
	prop_law1 :: E.Stream A -> Bool
	prop_law1 x = mappend mempty x == x
	
	prop_law2 :: E.Stream A -> Bool
	prop_law2 x = mappend x mempty == x
	
	prop_law3 :: E.Stream A -> E.Stream A -> E.Stream A -> Bool
	prop_law3 x y z = mappend x (mappend y z) == mappend (mappend x y) z
	
	prop_law4 :: [E.Stream A] -> Bool
	prop_law4 xs = mconcat xs == foldr mappend mempty xs

suite_StreamFunctor :: Suite
suite_StreamFunctor = suite "functor" props where
	props = [ property "law-1" prop_law1
	        , property "law-2" prop_law2
	        ]
	
	prop_law1 :: E.Stream A -> Bool
	prop_law1 x = fmap id x == id x
	
	prop_law2 :: E.Stream A -> Blind (B -> C) -> Blind (A -> B) -> Bool
	prop_law2 x (Blind f) (Blind g) = fmap (f . g) x == (fmap f . fmap g) x

suite_StreamMonad :: Suite
suite_StreamMonad = suite "Monad Stream" props where
	props = [ property "law-1" prop_law1
	        , property "law-2" prop_law2
	        , property "law-3" prop_law3
	        ]
	
	prop_law1 :: A -> Blind (A -> E.Stream B) -> Bool
	prop_law1 a (Blind f) = (return a >>= f) == f a
	
	prop_law2 :: E.Stream A -> Bool
	prop_law2 m = (m >>= return) == m
	
	prop_law3 :: E.Stream A -> Blind (A -> E.Stream B) -> Blind (B -> E.Stream C) -> Bool
	prop_law3 m (Blind f) (Blind g) = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

-- }}}

-- Generic properties {{{

test_Enumeratee :: T.Text -> E.Enumeratee A A Identity (Maybe A) -> Suite
test_Enumeratee name enee = suite name props where
	props = [ property "incremental" prop_incremental
	        , property "nest-errors" prop_nest_errors
	        ]
	
	prop_incremental (Positive n) (NonEmpty xs) = let
		result = runIdentity (E.run_ iter)
		expected = (Just (head xs), tail xs)
		
		iter = E.enumList n xs $$ do
			a <- E.joinI (enee $$ EL.head)
			b <- EL.consume
			return (a, b)
		
		in result == expected
	
	prop_nest_errors (Positive n) (NonEmpty xs) = let
		result = runIdentity (E.run_ iter)
		
		iter = E.enumList n xs $$ do
			_ <- enee $$ E.throwError (Exc.ErrorCall "")
			EL.consume
		
		in result == xs

-- }}}

-- Text encoding / decoding {{{

suite_Text :: Suite
suite_Text = suite "text"
	[ suite_Encoding
	, suite_Decoding
	]

suite_Encoding :: Suite
suite_Encoding = suite "encoding"
	[ suite_Encode_ASCII
	, suite_Encode_ISO8859
	]

suite_Encode_ASCII :: Suite
suite_Encode_ASCII = suite "ascii" props where
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

suite_Encode_ISO8859 :: Suite
suite_Encode_ISO8859 = suite "iso-8859-1" props where
	props = [ property "works" (forAll genISO8859 prop_works)
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

suite_Decoding :: Suite
suite_Decoding = suite "decoding"
	[ suite_Decode_ASCII
	, suite_Decode_UTF8
	, suite_Decode_UTF16_BE
	, suite_Decode_UTF16_LE
	, suite_Decode_UTF32_BE
	, suite_Decode_UTF32_LE
	]

suite_Decode_ASCII :: Suite
suite_Decode_ASCII = suite "ascii" props where
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

suite_Decode_UTF8 :: Suite
suite_Decode_UTF8 = suite "utf-8" props where
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

suite_Decode_UTF16_BE :: Suite
suite_Decode_UTF16_BE = suite "utf-16-be" props where
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

suite_Decode_UTF16_LE :: Suite
suite_Decode_UTF16_LE = suite "utf-16-le" props where
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

suite_Decode_UTF32_BE :: Suite
suite_Decode_UTF32_BE = suite "utf-32-be" props where
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

suite_Decode_UTF32_LE :: Suite
suite_Decode_UTF32_LE = suite "utf-32-le" props where
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

-- }}}

-- List analogues {{{

suite_ListAnalogues :: Suite
suite_ListAnalogues = suite "list-analogues"
	[ suite_Consume
	, suite_Head
	, suite_Drop
	, suite_Take
	, suite_Require
	, suite_Isolate
	, suite_SplitWhen
	, suite_Map
	, suite_ConcatMap
	, suite_MapM
	, suite_ConcatMapM
	, suite_MapAccum
	, suite_MapAccumM
	, suite_Filter
	, suite_FilterM
	]

check :: Eq b => E.Iteratee a Identity b -> ([a] -> Either Exc.ErrorCall b) -> [a] -> Bool
check iter plain xs = expected == run iter xs where
	expected = case plain xs of
		Left exc -> Left (Just exc)
		Right x -> Right x
	
	run iter xs = case runIdentity (E.run (E.enumList 1 xs $$ iter)) of
		Left exc -> Left (Exc.fromException exc)
		Right x -> Right x

testListAnalogue name iterList plainList iterText plainText iterBytes plainBytes = suite name tests where
	tests = [ property "list" prop_List
	        , property "text" prop_Text
	        , property "bytes" prop_Bytes
	        ]
	
	prop_List :: [A] -> Bool
	prop_List xs = check iterList plainList xs
	
	prop_Text xs = check iterText (plainText . TL.fromChunks) xs
	prop_Bytes xs = check iterBytes (plainBytes . BL.fromChunks) xs

testListAnalogueN name iterList plainList iterText plainText iterBytes plainBytes = suite name tests where
	tests = [ property "list" prop_List
	        , property "text" prop_Text
	        , property "bytes" prop_Bytes
	        ]
	
	prop_List :: Positive Integer -> [A] -> Bool
	prop_List (Positive n) xs = check (iterList n) (plainList n) xs
	
	prop_Text (Positive n) xs = check (iterText n) (plainText n . TL.fromChunks) xs
	prop_Bytes (Positive n) xs = check (iterBytes n) (plainBytes n . BL.fromChunks) xs

testListAnalogueX name iterList plainList iterText plainText iterBytes plainBytes = suite name tests where
	tests = [ property "list" prop_List
	        , property "text" prop_Text
	        , property "bytes" prop_Bytes
	        ]
	
	prop_List :: A -> [A] -> Bool
	prop_List x xs = check (iterList x) (plainList x) xs
	
	prop_Text x xs = check (iterText x) (plainText x . TL.fromChunks) xs
	prop_Bytes x xs = check (iterBytes x) (plainBytes x . BL.fromChunks) xs

suite_Consume :: Suite
suite_Consume = testListAnalogue "consume"
	EL.consume Right
	ET.consume Right
	EB.consume Right

suite_Head :: Suite
suite_Head = testListAnalogue "head"
	(do
		x <- EL.head
		extra <- EL.consume
		return (x, extra)
	)
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:xs') -> (Just x, xs'))
	(do
		x <- ET.head
		extra <- ET.consume
		return (x, extra)
	)
	(\text -> Right $ case TL.uncons text of
		Nothing -> (Nothing, TL.empty)
		Just (x, extra) -> (Just x, extra))
	(do
		x <- EB.head
		extra <- EB.consume
		return (x, extra)
	)
	(\bytes -> Right $ case BL.uncons bytes of
		Nothing -> (Nothing, BL.empty)
		Just (x, extra) -> (Just x, extra))

suite_Drop :: Suite
suite_Drop = testListAnalogueN "drop"
	(\n -> EL.drop n >> EL.consume)
	(\n -> Right . L.genericDrop n)
	(\n -> ET.drop n >> ET.consume)
	(\n -> Right . TL.drop (fromInteger n))
	(\n -> EB.drop n >> EB.consume)
	(\n -> Right . BL.drop (fromInteger n))

suite_Take :: Suite
suite_Take = testListAnalogueN "take"
	(\n -> do
		xs <- EL.take n
		extra <- EL.consume
		return (xs, extra))
	(\n -> Right . L.genericSplitAt n)
	(\n -> do
		xs <- ET.take n
		extra <- ET.consume
		return (xs, extra))
	(\n -> Right . TL.splitAt (fromInteger n))
	(\n -> do
		xs <- EB.take n
		extra <- EB.consume
		return (xs, extra))
	(\n -> Right . BL.splitAt (fromInteger n))

suite_Require :: Suite
suite_Require = testListAnalogueN "require"
	(\n -> do
		EL.require n
		EL.consume)
	(\n xs -> if n > toInteger (length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)
	(\n -> do
		ET.require n
		ET.consume)
	(\n xs -> if n > toInteger (TL.length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)
	(\n -> do
		EB.require n
		EB.consume)
	(\n xs -> if n > toInteger (BL.length xs)
		then Left (Exc.ErrorCall "require: Unexpected EOF")
		else Right xs)

suite_Isolate :: Suite
suite_Isolate = testListAnalogue "isolate"
	(do
		x <- E.joinI (EL.isolate 2 $$ EL.head)
		extra <- EL.consume
		return (x, extra))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:[]) -> (Just x, [])
		(x:_:xs') -> (Just x, xs'))
	(do
		x <- E.joinI (ET.isolate 2 $$ ET.head)
		extra <- ET.consume
		return (x, extra))
	(\text -> Right $ case TL.unpack text of
		[] -> (Nothing, TL.empty)
		(x:[]) -> (Just x, TL.empty)
		(x:_:xs') -> (Just x, TL.pack xs'))
	(do
		x <- E.joinI (EB.isolate 2 $$ EB.head)
		extra <- EB.consume
		return (x, extra))
	(\bytes -> Right $ case BL.unpack bytes of
		[] -> (Nothing, BL.empty)
		(x:[]) -> (Just x, BL.empty)
		(x:_:xs) -> (Just x, BL.pack xs))

suite_SplitWhen :: Suite
suite_SplitWhen = testListAnalogueX "splitWhen"
	(\x -> do
		xs <- E.joinI (EL.splitWhen (== x) $$ EL.consume)
		extra <- EL.consume
		return (xs, extra))
	(\x xs -> let
		split = LS.split . LS.dropFinalBlank . LS.dropDelims . LS.whenElt
		in Right (split (== x) xs, []))
	(\c -> do
		xs <- E.joinI (ET.splitWhen (== c) $$ EL.consume)
		extra <- EL.consume
		return (xs, extra))
	(\c text -> let
		split = LS.split . LS.dropFinalBlank . LS.dropDelims . LS.whenElt
		chars = TL.unpack text
		in Right (map T.pack (split (== c) chars), []))
	(\x -> do
		xs <- E.joinI (EB.splitWhen (== x) $$ EL.consume)
		extra <- EL.consume
		return (xs, extra))
	(\x bytes -> let
		split = LS.split . LS.dropFinalBlank . LS.dropDelims . LS.whenElt
		words = BL.unpack bytes
		in Right (map B.pack (split (== x) words), []))

suite_Map :: Suite
suite_Map = test_Enumeratee "map" (EL.map id)

suite_ConcatMap :: Suite
suite_ConcatMap = test_Enumeratee "concatMap" (EL.concatMap (:[]))

suite_MapM :: Suite
suite_MapM = test_Enumeratee "mapM" (EL.mapM return)

suite_ConcatMapM :: Suite
suite_ConcatMapM = test_Enumeratee "concatMapM" (EL.concatMapM (\x -> return [x]))

suite_MapAccum :: Suite
suite_MapAccum = testListAnalogue "mapAccum"
	(do
		let enee = EL.mapAccum (\s ao -> (s+1, (s, ao))) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EL.consume
		return (a, b))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:xs') -> (Just (10, x), xs'))
	(do
		let enee = ET.mapAccum (\s ao -> (s+1, succ ao)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- ET.consume
		return (a, b))
	(\text -> Right $ case TL.uncons text of
		Nothing -> (Nothing, TL.empty)
		Just (c, text') -> (Just (T.singleton (succ c)), text'))
	(do
		let enee = EB.mapAccum (\s ao -> (s+1, ao + s)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EB.consume
		return (a, b))
	(\bytes -> Right $ case BL.uncons bytes of
		Nothing -> (Nothing, BL.empty)
		Just (b, bytes') -> (Just (B.singleton (b + 10)), bytes'))


suite_MapAccumM :: Suite
suite_MapAccumM = testListAnalogue "mapAccumM"
	(do
		let enee = EL.mapAccumM (\s ao -> return (s+1, (s, ao))) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EL.consume
		return (a, b))
	(\xs -> Right $ case xs of
		[] -> (Nothing, [])
		(x:xs') -> (Just (10, x), xs'))
	(do
		let enee = ET.mapAccumM (\s ao -> return (s+1, succ ao)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- ET.consume
		return (a, b))
	(\text -> Right $ case TL.uncons text of
		Nothing -> (Nothing, TL.empty)
		Just (c, text') -> (Just (T.singleton (succ c)), text'))
	(do
		let enee = EB.mapAccumM (\s ao -> return (s+1, ao + s)) 10
		a <- E.joinI (enee $$ EL.head)
		b <- EB.consume
		return (a, b))
	(\bytes -> Right $ case BL.uncons bytes of
		Nothing -> (Nothing, BL.empty)
		Just (b, bytes') -> (Just (B.singleton (b + 10)), bytes'))

suite_Filter :: Suite
suite_Filter = test_Enumeratee "filter" (EL.filter (\_ -> True))

suite_FilterM :: Suite
suite_FilterM = test_Enumeratee "filterM" (EL.filterM (\_ -> return True))

-- }}}

-- Specific functions

suite_Other :: Suite
suite_Other = suite "other"
	[ test_Sequence
	, test_joinE
	, test_CatchError_WithoutContinue
	, test_CatchError_NotDivergent
	, test_CatchError_Interleaved
	]

test_Sequence :: Suite
test_Sequence = property "sequence" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = map Just xs
		
		iter = E.enumList n xs $$ E.joinI (E.sequence EL.head $$ EL.consume)

test_joinE :: Suite
test_joinE = property "joinE" prop where
	prop :: [Integer] -> Bool
	prop xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = map (* 10) xs
		
		iter = (E.joinE (E.enumList 1 xs) (EL.map (* 10))) $$ EL.consume

test_CatchError_WithoutContinue :: Suite
test_CatchError_WithoutContinue = property "catchError/without-continue" test where
	test = case runIdentity (E.run (E.enumList 1 [] $$ iter)) of
		Left err -> Exc.fromException err == Just (Exc.ErrorCall "require: Unexpected EOF")
		Right _ -> False
	iter = E.catchError
		(E.throwError (Exc.ErrorCall "error"))
		(\_ -> EL.require 1)

test_CatchError_NotDivergent :: Suite
test_CatchError_NotDivergent = property "catchError/not-divergent" test where
	test = case runIdentity (E.run (E.enumList 1 [] $$ iter)) of
		Left err -> Exc.fromException err == Just (Exc.ErrorCall "require: Unexpected EOF")
		Right _ -> False
	iter = E.catchError
		(do
			EL.head
			E.throwError (Exc.ErrorCall "error"))
		(\_ -> EL.require 1)

test_CatchError_Interleaved :: Suite
test_CatchError_Interleaved = property "catchError/interleaved" prop where
	prop = within 1000000 (morallyDubiousIOProperty io)
	io = do
		mvar <- newEmptyMVar
		E.run_ (enumMVar mvar $$ E.catchError (iter mvar) onError)
	enumMVar mvar = loop where
		loop (E.Continue k) = do
			x <- liftIO (takeMVar mvar)
			k (E.Chunks [x]) >>== loop
		loop step = E.returnI step
	iter mvar = do
		liftIO (putMVar mvar ())
		EL.head
		return True
	onError err = return False

-- misc

genASCII :: IsString a => Gen a
genASCII = fmap fromString string where
	string = sized $ \n -> do
		k <- choose (0,n)
		sequence [ char | _ <- [1..k] ]
	
	char = chr `fmap` choose (0,0x7F)

genISO8859 :: IsString a => Gen a
genISO8859 = fmap fromString string where
	string = sized $ \n -> do
		k <- choose (0,n)
		sequence [ char | _ <- [1..k] ]
	
	char = chr `fmap` choose (0,0xFF)

genUnicode :: IsString a => Gen a
genUnicode = fmap fromString string where
	string = sized $ \n -> do
		k <- choose (0,n)
		sequence [ char | _ <- [1..k] ]
	
	excluding :: [a -> Bool] -> Gen a -> Gen a
	excluding bad gen = loop where
		loop = do
			x <- gen
			if or (map ($ x) bad)
				then loop
				else return x
	
	reserved = [lowSurrogate, highSurrogate, noncharacter]
	lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
	highSurrogate c = c >= 0xD800 && c <= 0xDBFF
	noncharacter c = masked == 0xFFFE || masked == 0xFFFF where
		masked = c .&. 0xFFFF
	
	ascii = choose (0,0x7F)
	plane0 = choose (0xF0, 0xFFFF)
	plane1 = oneof [ choose (0x10000, 0x10FFF)
	               , choose (0x11000, 0x11FFF)
	               , choose (0x12000, 0x12FFF)
	               , choose (0x13000, 0x13FFF)
	               , choose (0x1D000, 0x1DFFF)
	               , choose (0x1F000, 0x1FFFF)
	               ]
	plane2 = oneof [ choose (0x20000, 0x20FFF)
	               , choose (0x21000, 0x21FFF)
	               , choose (0x22000, 0x22FFF)
	               , choose (0x23000, 0x23FFF)
	               , choose (0x24000, 0x24FFF)
	               , choose (0x25000, 0x25FFF)
	               , choose (0x26000, 0x26FFF)
	               , choose (0x27000, 0x27FFF)
	               , choose (0x28000, 0x28FFF)
	               , choose (0x29000, 0x29FFF)
	               , choose (0x2A000, 0x2AFFF)
	               , choose (0x2B000, 0x2BFFF)
	               , choose (0x2F000, 0x2FFFF)
	               ]
	plane14 = choose (0xE0000, 0xE0FFF)
	planes = [ascii, plane0, plane1, plane2, plane14]
	
	char = chr `fmap` excluding reserved (oneof planes)

instance Arbitrary a => Arbitrary (E.Stream a) where
	arbitrary = frequency
		[ (10, return E.EOF)
		, (90, fmap E.Chunks arbitrary)
		]

instance Arbitrary T.Text where
	arbitrary = genUnicode

instance Arbitrary B.ByteString where
	arbitrary = genUnicode

instance Eq Exc.ErrorCall where
	(Exc.ErrorCall s1) == (Exc.ErrorCall s2) = s1 == s2
