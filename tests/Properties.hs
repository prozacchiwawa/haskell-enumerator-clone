-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Poly
import qualified Test.Framework as F
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char (chr)
import Data.Monoid
import Data.Functor.Identity
import Data.String

tests :: [F.Test]
tests =
	[ test_StreamInstances
	, test_Primitives
	, test_Text
	, test_ListAnalogues
	, test_Functions
	]

main :: IO ()
main = F.defaultMain tests

-- Stream instances {{{

test_StreamInstances :: F.Test
test_StreamInstances = F.testGroup "Stream Instances"
	[ test_StreamMonoid
	, test_StreamFunctor
	, test_StreamApplicative
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

test_StreamApplicative :: F.Test
test_StreamApplicative = F.testGroup "Applicative Stream" props where
	props = [ testProperty "law 1" prop_law1
	        , testProperty "law 2" prop_law2
	        , testProperty "law 3" prop_law3
	        , testProperty "law 4" prop_law4
	        , testProperty "law 5" prop_law5
	        ]
	
	prop_law1 :: E.Stream A -> Bool
	prop_law1 v = (pure id <*> v) == v
	
	prop_law2 :: Blind (E.Stream (B -> C)) -> Blind (E.Stream (A -> B)) -> E.Stream A -> Bool
	prop_law2 (Blind u) (Blind v) w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
	
	prop_law3 :: Blind (A -> B) -> A -> Bool
	prop_law3 (Blind f) x = (pure f <*> pure x) == (pure (f x) `asTypeOf` E.Chunks [B 0])
	
	prop_law4 :: Blind (E.Stream (A -> B)) -> A -> Bool
	prop_law4 (Blind u) y = (u <*> pure y) == (pure ($ y) <*> u)
	
	prop_law5 :: Blind (A -> B) -> E.Stream A -> Bool
	prop_law5 (Blind f) x = (fmap f x) == (pure f <*> x)

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
			a <- E.joinI (enee $$ E.head)
			b <- E.consume
			return (a, b)
		
		in result == expected
	
	prop_nest_errors (Positive n) (NonEmpty xs) = let
		result = runIdentity (E.run_ iter)
		
		iter = E.enumList n xs $$ do
			a <- enee $$ E.throwError (ErrorCall "")
			E.consume
		
		in result == xs

-- }}}

-- Primitives {{{

test_Primitives :: F.Test
test_Primitives = F.testGroup "Primitives"
	[ test_Map
	, test_ConcatMap
	, test_MapM
	, test_ConcatMapM
	, test_Filter
	, test_FilterM
	]

test_Map :: F.Test
test_Map = test_Enumeratee "map" (E.map id)

test_ConcatMap :: F.Test
test_ConcatMap = test_Enumeratee "concatMap" (E.concatMap (:[]))

test_MapM :: F.Test
test_MapM = test_Enumeratee "mapM" (E.mapM return)

test_ConcatMapM :: F.Test
test_ConcatMapM = test_Enumeratee "concatMapM" (E.concatMapM (\x -> return [x]))

test_Filter :: F.Test
test_Filter = test_Enumeratee "filter" (E.filter (\_ -> True))

test_FilterM :: F.Test
test_FilterM = test_Enumeratee "filterM" (E.filterM (\_ -> return True))

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
		Right result = encode E.consume (map T.singleton chars)
		
		chars = B8.unpack bytes
		words = B.unpack bytes
	
	prop_error = isLeft (encode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [T.pack "\x61\xFF"]
	
	prop_lazy = either (const False) (== expected) result where
		result = encode E.head input
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
		Right result = encode E.consume (map T.singleton chars)
		
		chars = B8.unpack bytes
		words = B.unpack bytes
	
	prop_error = isLeft (encode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [T.pack "\x61\xFF5E"]
	
	prop_lazy = either (const False) (== expected) result where
		result = encode E.head input
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
		Right result = decode E.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf8 text)
		chars = T.unpack text
	
	prop_error = isLeft (decode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0xFF]]
	
	prop_lazy = either (const False) (== expected) result where
		result = decode E.head input
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
		Right result = decode E.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf8 text)
		chars = T.unpack text
	
	prop_error = isLeft (decode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0x61, 0x80]]
	
	prop_lazy = either (const False) (== expected) result where
		result = decode E.head input
		input = [B.pack [0x61, 0x80]]
		expected = Just (T.pack "a")
	
	prop_incremental = either (const False) (== expected) result where
		result = decode E.head input
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
		Right result = decode E.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf16BE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode E.head input
		input = [B.pack [0x00, 0x61, 0xDD, 0x1E]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0x00, 0x61, 0xDD, 0x1E]]
	
	prop_incremental = either (const False) (== expected) result where
		result = decode E.head input
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
		Right result = decode E.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf16LE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode E.head input
		input = [B.pack [0x61, 0x00, 0x1E, 0xDD]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0x61, 0x00, 0x1E, 0xDD]]
	
	prop_incremental = either (const False) (== expected) result where
		result = decode E.head input
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
		Right result = decode E.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf32BE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode E.head input
		input = [B.pack [0x00, 0x00, 0x00, 0x61, 0xFF, 0xFF]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode E.consume input)  where
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
		Right result = decode E.consume (map B.singleton bytes)
		
		bytes = B.unpack (TE.encodeUtf32LE text)
		chars = T.unpack text
	
	prop_lazy = either (const False) (== expected) result where
		result = decode E.head input
		input = [B.pack [0x61, 0x00, 0x00, 0x00, 0xFF, 0xFF]]
		expected = Just (T.pack "a")
	
	prop_error = isLeft (decode E.consume input)  where
		isLeft = either (const True) (const False)
		input = [B.pack [0xFF, 0xFF, 0xFF, 0xFF]]

-- }}}

-- List analogues {{{

test_ListAnalogues :: F.Test
test_ListAnalogues = F.testGroup "list analogues"
	[ test_ListConsume
	, test_ListHead
	, test_ListDrop
	, test_ListTake
	-- , test_ListPeek
	, test_ListRequire
	]

test_ListConsume :: F.Test
test_ListConsume = testProperty "List.consume" prop where
	prop :: [A] -> Bool
	prop xs = result == xs where
		result = runIdentity (E.run_ iter)
		iter = E.enumList 1 xs $$ EL.consume

test_ListHead :: F.Test
test_ListHead = testProperty "List.head" prop where
	prop :: [A] -> Bool
	prop xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = case xs of
			[] -> (Nothing, [])
			(x:xs') -> (Just x, xs')
		
		iter = E.enumList 1 xs $$ do
			x <- EL.head
			extra <- EL.consume
			return (x, extra)

test_ListDrop :: F.Test
test_ListDrop = testProperty "List.drop" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = drop (fromInteger n) xs
		
		iter = E.enumList 1 xs $$ do
			EL.drop n
			EL.consume

test_ListTake :: F.Test
test_ListTake = testProperty "List.take" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = splitAt (fromInteger n) xs
		
		iter = E.enumList 1 xs $$ do
			xs <- EL.take n
			extra <- EL.consume
			return (xs, extra)

{-
test_ListPeek :: F.Test
test_ListPeek = testProperty "List.peek" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = runIdentity (E.run_ iter)
		expected = (take (fromInteger n) xs, xs)
		
		iter = E.enumList 1 xs $$ do
			xs <- EL.peek n
			extra <- EL.consume
			return (xs, extra)
-}

test_ListRequire :: F.Test
test_ListRequire = testProperty "List.require" prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) xs = result == expected where
		result = case runIdentity (E.run iter) of
			Left exc -> Left (show exc)
			Right x -> Right x
		expected = if n > toInteger (length xs)
			then Left "require: Unexpected EOF"
			else Right xs
		
		iter = E.enumList 1 xs $$ do
			EL.require n
			EL.consume
-- }}}

-- Specific functions

test_Functions :: F.Test
test_Functions = F.testGroup "Functions"
	[ testProperty "consume" prop_Consume
	, testProperty "sequence" prop_Sequence
	]

prop_Consume :: Property
prop_Consume = property (\n xs -> runIdentity (E.run_ (iter n xs)) == xs) where
	iter :: Positive Integer -> [A] -> E.Iteratee A Identity [A]
	iter (Positive n) xs = E.enumList n xs $$ E.consume

prop_Sequence :: Property
prop_Sequence = property (\n xs -> runIdentity (E.run_ (iter n xs)) == map Just xs) where
	iter :: Positive Integer -> [A] -> E.Iteratee A Identity [Maybe A]
	iter (Positive n) xs = E.enumList n xs $$ E.joinI (E.sequence E.head $$ E.consume)

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
