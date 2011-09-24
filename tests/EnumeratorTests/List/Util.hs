{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Util
	( test_Enumeratee
	, prop_List
	, prop_ListN
	, prop_ListX
	) where

import qualified Control.Exception as Exc
import           Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)
import           Test.QuickCheck.Poly (A)

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util (check)

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

prop_List :: Eq b
          => E.Iteratee A Identity b
          -> ([A] -> Either Exc.ErrorCall b)
          -> [A]
          -> Bool
prop_List iter plain = prop where
	prop :: [A] -> Bool
	prop = check iter plain

prop_ListN :: Eq b
           => (Integer -> E.Iteratee A Identity b)
           -> (Integer -> [A] -> Either Exc.ErrorCall b)
           -> Positive Integer
           -> [A]
           -> Bool
prop_ListN iter plain = prop where
	prop :: Positive Integer -> [A] -> Bool
	prop (Positive n) = check (iter n) (plain n)

prop_ListX :: Eq b
           => (A -> E.Iteratee A Identity b)
           -> (A -> [A] -> Either Exc.ErrorCall b)
           -> A
           -> [A]
           -> Bool
prop_ListX iter plain = prop where
	prop :: A -> [A] -> Bool
	prop x = check (iter x) (plain x)
