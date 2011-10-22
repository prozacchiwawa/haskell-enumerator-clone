{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.List.Fold
	( test_Fold
	, test_FoldM
	) where

import qualified Control.Exception as Exception
import           Control.Monad (foldM)
import           Data.Functor.Identity (runIdentity)
import           Data.List (foldl')
import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck.Poly
import           Test.QuickCheck.Modifiers

import qualified Data.Enumerator as E
import           Data.Enumerator (($$))
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.List.Util ()

test_Fold :: Suite
test_Fold = suite "fold"
	[ property "model" prop_Fold
	, test_FoldStrict
	, test_Fold_EOF
	]

prop_Fold :: Blind (B -> A -> B) -> B -> [A] -> Bool
prop_Fold (Blind f) z xs = result == expected where
	result = E.runLists_ [xs] (EL.fold f z)
	expected = foldl' f z xs

test_FoldStrict :: Suite
test_FoldStrict = assertions "strict" $ do
	let exc = Exception.ErrorCall "fail-step"
	let step _ x = case x of
		'C' -> Exception.throw exc
		_ -> 'a'
	$expect $ throwsEq exc (E.run_ (E.enumList 1 ['A', 'B', 'C'] $$ EL.fold step 'a'))
	$expect $ throwsEq exc (E.run_ (E.enumList 3 ['A', 'B', 'C'] $$ EL.fold step 'a'))

test_Fold_EOF :: Suite
test_Fold_EOF = assertions "eof" $ do
	$expect $ equal
		Nothing
		(E.runLists_ [] $ do
			_ <- EL.fold (++) ['A']
			EL.head)

test_FoldM :: Suite
test_FoldM = suite "foldM"
	[ property "model" prop_FoldM
	, test_FoldM_EOF
	]

prop_FoldM :: Blind (B -> A -> B) -> B -> [A] -> Bool
prop_FoldM (Blind f) z xs = result == expected where
	result = E.runLists_ [xs] (EL.foldM f' z)
	expected = runIdentity (foldM f' z xs)
	f' b a = return (f b a)

test_FoldM_EOF :: Suite
test_FoldM_EOF = assertions "eof" $ do
	$expect $ equal
		Nothing
		(E.runLists_ [] $ do
			_ <- EL.foldM (\x y -> return (x ++ y)) ['A']
			EL.head)
