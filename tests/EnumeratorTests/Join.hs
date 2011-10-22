{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Join
	( test_JoinE
	, test_JoinI
	, test_JoinOperatorAssociativity
	) where

import           Control.Exception
import           Data.Char (toLower)
import           Data.Functor.Identity (Identity, runIdentity)

import           Test.Chell

import           Data.Enumerator (($$), ($=), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util (equalExc)

test_JoinE :: Suite
test_JoinE = suite "joinE"
	[ test_JoinE_Success
	, test_JoinE_Error
	, test_JoinE_Divergent
	]

test_JoinE_Success :: Suite
test_JoinE_Success = assertions "success" $ do
	let enum :: Monad m => E.Enumerator Char m b
	    enum = E.joinE (E.enumLists [['A', 'B', 'C']]) (EL.map toLower)
	$expect $ equal
		['a', 'b', 'c']
		(runIdentity (E.run_ (enum $$ EL.consume)))

test_JoinE_Error :: Suite
test_JoinE_Error = assertions "error" $ do
	let enum :: Monad m => E.Enumerator Char m b
	    enum = E.joinE (E.enumLists [['A', 'B', 'C']]) (E.sequence (E.throwError (ErrorCall "foo")))
	
	$expect $ equalExc
		(ErrorCall "foo")
		(runIdentity (E.run (enum $$ EL.consume)))

test_JoinE_Divergent :: Suite
test_JoinE_Divergent = assertions "divergent" $ do
	let enum :: Monad m => E.Enumerator Char m b
	    enum = E.joinE (E.enumLists [['A', 'B', 'C']]) (EL.map toLower)
	let diverg :: Monad m => E.Iteratee a m b
	    diverg = E.continue (\_ -> diverg)
	
	$expect $ throwsEq
		(ErrorCall "enumEOF: divergent iteratee")
		(E.run_ (enum $$ diverg))

test_JoinI :: Suite
test_JoinI = assertions "joinI" $ do
	let enum :: Monad m => E.Enumerator Char m b
	    enum = E.enumLists [['A', 'B', 'C']]
	
	let diverg :: Monad m => E.Iteratee a m b
	    diverg = E.continue (\_ -> diverg)
	
	$expect $ equal
		['a', 'b', 'c']
		(runIdentity (E.run_ (enum $$ E.joinI (EL.map toLower $$ EL.consume))))
	$expect $ equalExc
		(ErrorCall "foo")
		(runIdentity (E.run (enum $$ E.joinI (EL.map toLower $$ E.throwError (ErrorCall "foo")))))
	$expect $ throwsEq
		(ErrorCall "joinI: divergent iteratee")
		(E.run_ (enum $$ E.joinI (EL.map toLower $$ diverg)))

test_JoinOperatorAssociativity :: Suite
test_JoinOperatorAssociativity = assertions "join-operator-associativity" $ do
	let xs = ['A', 'B', 'C']
	let enum = E.enumList 1 xs
	let enee = EL.map id
	let iter = EL.consume
	xs1 <- E.run_ $ enum $$ enee =$ enee =$ iter
	xs2 <- E.run_ $ enum $= enee $$ enee =$ iter
	xs3 <- E.run_ $ enum $= enee $= enee $$ iter
	$expect (equal xs xs1)
	$expect (equal xs xs2)
	$expect (equal xs xs3)
	return ()
