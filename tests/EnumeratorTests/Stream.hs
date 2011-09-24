{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010-2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Stream
	( test_Stream
	) where

import           Data.Monoid (mappend, mempty, mconcat)

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)
import           Test.QuickCheck.Poly (A, B, C)

import qualified Data.Enumerator as E

import           EnumeratorTests.Util ()

test_Stream :: Suite
test_Stream = suite "stream"
	[ test_Monoid
	, test_Functor
	, test_Monad
	]

test_Monoid :: Suite
test_Monoid = suite "monoid"
	[ property "law-1" prop_MonoidLaw1
	, property "law-2" prop_MonoidLaw2
	, property "law-3" prop_MonoidLaw3
	, property "law-4" prop_MonoidLaw4
	]

prop_MonoidLaw1 :: E.Stream A -> Bool
prop_MonoidLaw1 x = mappend mempty x == x

prop_MonoidLaw2 :: E.Stream A -> Bool
prop_MonoidLaw2 x = mappend x mempty == x

prop_MonoidLaw3 :: E.Stream A -> E.Stream A -> E.Stream A -> Bool
prop_MonoidLaw3 x y z = mappend x (mappend y z) == mappend (mappend x y) z

prop_MonoidLaw4 :: [E.Stream A] -> Bool
prop_MonoidLaw4 xs = mconcat xs == foldr mappend mempty xs

test_Functor :: Suite
test_Functor = suite "functor"
	[ property "law-1" prop_FunctorLaw1
	, property "law-2" prop_FunctorLaw2
	]

prop_FunctorLaw1 :: E.Stream A -> Bool
prop_FunctorLaw1 x = fmap id x == id x

prop_FunctorLaw2 :: E.Stream A -> Blind (B -> C) -> Blind (A -> B) -> Bool
prop_FunctorLaw2 x (Blind f) (Blind g) = fmap (f . g) x == (fmap f . fmap g) x

test_Monad :: Suite
test_Monad = suite "monad"
	[ property "law-1" prop_MonadLaw1
	, property "law-2" prop_MonadLaw2
	, property "law-3" prop_MonadLaw3
	]

prop_MonadLaw1 :: A -> Blind (A -> E.Stream B) -> Bool
prop_MonadLaw1 a (Blind f) = (return a >>= f) == f a

prop_MonadLaw2 :: E.Stream A -> Bool
prop_MonadLaw2 m = (m >>= return) == m

prop_MonadLaw3 :: E.Stream A -> Blind (A -> E.Stream B) -> Blind (B -> E.Stream C) -> Bool
prop_MonadLaw3 m (Blind f) (Blind g) = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
