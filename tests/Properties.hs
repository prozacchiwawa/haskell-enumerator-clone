-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import qualified Control.Exception as Exc
import           Data.Bits ((.&.))
import           Data.Char (chr)
import qualified Data.List as L
import qualified Data.List.Split as LS
import           Data.Monoid (mappend, mempty, mconcat)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.String (IsString, fromString)
import           Data.Word (Word8)

import           Data.Enumerator (($$))
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

import           Test.QuickCheck hiding ((.&.))
import           Test.QuickCheck.Poly (A, B, C)
import qualified Test.Framework as F
import           Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [F.Test]
tests =
	[ test_StreamInstances
	, test_Text
	, test_ListAnalogues
	, test_Other
	]

main :: IO ()
main = F.defaultMain tests

-- Stream instances {{{

test_StreamInstances :: F.Test
test_StreamInstances = F.testGroup "Stream Instances"
	[ test_StreamMonoid
	, test_StreamFunctor
	, test_StreamMonad
	]

test_StreamMonoid :: F.Test
test_StreamMonoid = F.testGroup "Monoid Stream" props where
	props = [ testProperty "law 1" prop_law1
	        , testProperty "law 2" prop_law2
	        , testProperty "law 3" prop_law3
	        , testProperty "law 4" prop_law4
	        ]
	
	prop_law1 :: E.Stream A -> Bool
	prop_law1 x = mappend mempty x == x
	
	prop_law2 :: E.Stream A -> Bool
	prop_law2 x = mappend x mempty == x
	
	prop_law3 :: E.Stream A -> E.Stream A -> E.Stream A -> Bool
	prop_law3 x y z = mappend x (mappend y z) == mappend (mappend x y) z
	
	prop_law4 :: [E.Stream A] -> Bool
	prop_law4 xs = mconcat xs == foldr mappend mempty xs

test_StreamFunctor :: F.Test
test_StreamFunctor = F.testGroup "Functor Stream" props where
	props = [ testProperty "law 1" prop_law1
	        , testProperty "law 2" prop_law2
	        ]
	
	prop_law1 :: E.Stream A -> Bool
	prop_law1 x = fmap id x == id x
	
	prop_law2 :: E.Stream A -> Blind (B -> C) -> Blind (A -> B) -> Bool
	prop_law2 x (Blind f) (Blind g) = fmap (f . g) x == (fmap f . fmap g) x

test_StreamMonad :: F.Test
test_StreamMonad = F.testGroup "Monad Stream" props where
	props = [ testProperty "law 1" prop_law1
	        , testProperty "law 2" prop_law2
	        , testProperty "law 3" prop_law3
	        ]
	
	prop_law1 :: A -> Blind (A -> E.Stream B) -> Bool
	prop_law1 a (Blind f) = (return a >>= f) == f a
	
	prop_law2 :: E.Stream A -> Bool
	prop_law2 m = (m >>= return) == m
	
	prop_law3 :: E.Stream A -> Blind (A -> E.Stream B) -> Blind (B -> E.Stream C) -> Bool
	prop_law3 m (Blind f) (Blind g) = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

-- }}}

-- Generic properties {{{

test_Enumeratee :: String -> E.Enumeratee A A Identity (Maybe A) -> F.Test
test_Enumeratee name enee = F.testGroup name props where
	props = [ testProperty "incremental" prop_incremental
	        , testProperty "nest errors" prop_nest_errors
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

test_Text :: F.Test
test_Text = F.testGroup "Text"
	[ test_Encoding
	, test_Decoding
	]

test_Encoding :: F.Test
test_Encoding = F.testGroup "Encoding"
	[ test_Encode_ASCII
	, test_Encode_ISO8859
	]

test_Encode_ASCII :: F.Test
test_Encode_ASCII = F.testGroup "ASCII" props where
	props = [ testProperty "works" (forAll genASCII prop_works)
	        , testProperty "error" prop_error
	        , testProperty "lazy" prop_lazy
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

test_Encode_ISO8859 :: F.Test
test_Encode_ISO8859 = F.testGroup "ISO-8859-1" props where
	props = [ testProperty "works" (forAll genISO8859 prop_works)
	        , testProperty "error" prop_error
	        , testProperty "lazy" prop_lazy
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

test_Decoding :: F.Test
test_Decoding = F.testGroup "Decoding"
	[ test_Decode_ASCII
	, test_Decode_UTF8
	, test_Decode_UTF16_BE
	, test_Decode_UTF16_LE
	, test_Decode_UTF32_BE
	, test_Decode_UTF32_LE
	]

test_Decode_ASCII :: F.Test
test_Decode_ASCII = F.testGroup "ASCII" props where
	props = [ testProperty "works" (forAll genASCII prop_works)
	        , testProperty "error" prop_error
	        , testProperty "lazy" prop_lazy
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

test_Decode_UTF8 :: F.Test
test_Decode_UTF8 = F.testGroup "UTF-8" props where
	props = [ testProperty "works" prop_works
	        , testProperty "error" prop_error
	        , testProperty "lazy" prop_lazy
	        , testProperty "incremental" prop_incremental
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

test_Decode_UTF16_BE :: F.Test
test_Decode_UTF16_BE = F.testGroup "UTF-16-BE" props where
	props = [ testProperty "works" prop_works
	        , testProperty "lazy" prop_lazy
	        , testProperty "error" prop_error
	        , testProperty "incremental" prop_incremental
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

test_Decode_UTF16_LE :: F.Test
test_Decode_UTF16_LE = F.testGroup "UTF-16-LE" props where
	props = [ testProperty "works" prop_works
	        , testProperty "lazy" prop_lazy
	        , testProperty "error" prop_error
	        , testProperty "incremental" prop_incremental
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

test_Decode_UTF32_BE :: F.Test
test_Decode_UTF32_BE = F.testGroup "UTF-32-BE" props where
	props = [ testProperty "works" prop_works
	        , testProperty "lazy" prop_lazy
	        , testProperty "error" prop_error
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

test_Decode_UTF32_LE :: F.Test
test_Decode_UTF32_LE = F.testGroup "UTF-32-LE" props where
	props = [ testProperty "works" prop_works
	        , testProperty "lazy" prop_lazy
	        , testProperty "error" prop_error
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

test_ListAnalogues :: F.Test
test_ListAnalogues = F.testGroup "list analogues"
	[ test_Consume
	, test_Head
	, test_Drop
	, test_Take
	, test_Require
	, test_Isolate
	, test_SplitWhen
	, test_Map
	, test_ConcatMap
	, test_MapM
	, test_ConcatMapM
	, test_MapAccum
	, test_MapAccumM
	, test_Filter
	, test_FilterM
	]

check :: Eq b => E.Iteratee a Identity b -> ([a] -> Either Exc.ErrorCall b) -> [a] -> Bool
check iter plain xs = expected == run iter xs where
	expected = case plain xs of
		Left exc -> Left (Just exc)
		Right x -> Right x
	
	run iter xs = case runIdentity (E.run (E.enumList 1 xs $$ iter)) of
		Left exc -> Left (Exc.fromException exc)
		Right x -> Right x

testListAnalogue name iterList plainList iterText plainText iterBytes plainBytes = F.testGroup name tests where
	tests = [ testProperty "list" prop_List
	        , testProperty "text" prop_Text
	        , testProperty "bytes" prop_Bytes
	        ]
	
	prop_List :: [A] -> Bool
	prop_List xs = check iterList plainList xs
	
	prop_Text xs = check iterText (plainText . TL.fromChunks) xs
	prop_Bytes xs = check iterBytes (plainBytes . BL.fromChunks) xs

testListAnalogueN name iterList plainList iterText plainText iterBytes plainBytes = F.testGroup name tests where
	tests = [ testProperty "list" prop_List
	        , testProperty "text" prop_Text
	        , testProperty "bytes" prop_Bytes
	        ]
	
	prop_List :: Positive Integer -> [A] -> Bool
	prop_List (Positive n) xs = check (iterList n) (plainList n) xs
	
	prop_Text (Positive n) xs = check (iterText n) (plainText n . TL.fromChunks) xs
	prop_Bytes (Positive n) xs = check (iterBytes n) (plainBytes n . BL.fromChunks) xs

testListAnalogueX name iterList plainList iterText plainText iterBytes plainBytes = F.testGroup name tests where
	tests = [ testProperty "list" prop_List
	        , testProperty "text" prop_Text
	        , testProperty "bytes" prop_Bytes
	        ]
	
	prop_List :: A -> [A] -> Bool
	prop_List x xs = check (iterList x) (plainList x) xs
	
	prop_Text x xs = check (iterText x) (plainText x . TL.fromChunks) xs
	prop_Bytes x xs = check (iterBytes x) (plainBytes x . BL.fromChunks) xs

test_Consume :: F.Test
test_Consume = testListAnalogue "consume"
	EL.consume Right
	ET.consume Right
	EB.consume Right

test_Head :: F.Test
test_Head = testListAnalogue "head"
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

test_Drop :: F.Test
test_Drop = testListAnalogueN "drop"
	(\n -> EL.drop n >> EL.consume)
	(\n -> Right . L.genericDrop n)
	(\n -> ET.drop n >> ET.consume)
	(\n -> Right . TL.drop (fromInteger n))
	(\n -> EB.drop n >> EB.consume)
	(\n -> Right . BL.drop (fromInteger n))

test_Take :: F.Test
test_Take = testListAnalogueN "take"
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

test_Require :: F.Test
test_Require = testListAnalogueN "require"
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

test_Isolate :: F.Test
test_Isolate = testListAnalogue "isolate"
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

test_SplitWhen :: F.Test
test_SplitWhen = testListAnalogueX "splitWhen"
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

test_Map :: F.Test
test_Map = test_Enumeratee "map" (EL.map id)

test_ConcatMap :: F.Test
test_ConcatMap = test_Enumeratee "concatMap" (EL.concatMap (:[]))

test_MapM :: F.Test
test_MapM = test_Enumeratee "mapM" (EL.mapM return)

test_ConcatMapM :: F.Test
test_ConcatMapM = test_Enumeratee "concatMapM" (EL.concatMapM (\x -> return [x]))

test_MapAccum :: F.Test
test_MapAccum = testListAnalogue "mapAccum"
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


test_MapAccumM :: F.Test
test_MapAccumM = testListAnalogue "mapAccumM"
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

test_Filter :: F.Test
test_Filter = test_Enumeratee "filter" (EL.filter (\_ -> True))

test_FilterM :: F.Test
test_FilterM = test_Enumeratee "filterM" (EL.filterM (\_ -> return True))

-- }}}

-- Specific functions

test_Other :: F.Test
test_Other = F.testGroup "Other"
	[ test_Sequence
	, test_joinE
	]

test_Sequence :: F.Test
test_Sequence = testProperty "sequence" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = map Just xs
		
		iter = E.enumList n xs $$ E.joinI (E.sequence EL.head $$ EL.consume)

test_joinE :: F.Test
test_joinE = testProperty "joinE" prop where
	prop :: [Integer] -> Bool
	prop xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = map (* 10) xs
		
		iter = (E.joinE (E.enumList 1 xs) (EL.map (* 10))) $$ EL.consume

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
