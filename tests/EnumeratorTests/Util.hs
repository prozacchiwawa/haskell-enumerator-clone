{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Util
	( check
	, equalExc
	, todo
	, within
	, genASCII
	, genISO8859_1
	, genUnicode
	) where

import qualified Control.Exception as Exc
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import           Data.Char (chr)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           System.Timeout (timeout)

import           Test.Chell
import           Test.QuickCheck hiding ((.&.), within)

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E

check :: Eq b => E.Iteratee a Identity b -> ([a] -> Either Exc.ErrorCall b) -> [a] -> Bool
check iter plain xs = expected == run where
	expected = case plain xs of
		Left exc -> Left (Just exc)
		Right x -> Right x
	
	run = case runIdentity (E.run (E.enumList 1 xs $$ iter)) of
		Left exc -> Left (Exc.fromException exc)
		Right x -> Right x

todo :: T.Text -> Suite
todo name = skipIf True (assertions name (return ()))

genASCII :: IsString a => Gen a
genASCII = fmap fromString string where
	string = sized $ \n -> do
		k <- choose (0,n)
		sequence [ char | _ <- [1..k] ]
	
	char = chr `fmap` choose (0,0x7F)

genISO8859_1 :: IsString a => Gen a
genISO8859_1 = fmap fromString string where
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

-- | Require a test to complete within /n/ milliseconds.
within :: Int -> Suite -> Suite
within time s = suite (suiteName s) (map wrapTest (suiteTests s)) where
	wrapTest (Test name io) = test $ Test name $ \opts -> do
		res <- timeout (time * 1000) (io opts)
		case res of
			Just res' -> return res'
			Nothing -> return (TestAborted [] (T.pack ("Test timed out after " ++ show time ++ " milliseconds")))

equalExc :: (Eq exc, Exc.Exception exc) => exc -> Either Exc.SomeException a -> Assertion
equalExc expected funResult = Assertion (return result) where
	failed :: String -> AssertionResult
	failed str = AssertionFailed (T.pack ("equalExc: " ++ show str))
	result = case funResult of
		Right _ -> failed "received Right"
		Left exc -> case Exc.fromException exc of
			Nothing -> failed ("received unexpected exception: " ++ show exc)
			Just exc' -> if expected == exc'
				then AssertionPassed
				else failed (show expected ++ " /= " ++ show exc')
