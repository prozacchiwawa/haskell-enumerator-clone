{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Misc
	( test_ConcatEnums
	, test_Last
	, test_Length
	, test_LiftTrans
	, test_Peek
	, test_TryIO
	, test_PrintChunks
	) where

import           Control.Exception
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           Test.Chell

#ifdef MIN_VERSION_silently

import           System.IO.Silently (capture)

#endif

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util ()

test_ConcatEnums :: Suite
test_ConcatEnums = assertions "concatEnums" $ do
	let enum = E.concatEnums
		[ E.enumList 1 ['A']
		, E.enumList 1 ['B']
		, E.enumList 1 ['C']
		]
	$expect $ equal
		['A', 'B', 'C']
		(runIdentity (E.run_ (enum $$ EL.consume)))

test_Last :: Suite
test_Last = assertions "last" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(runIdentity (E.run_ (E.enumList 1 [] $$ do
			x <- E.last
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(Just 'E', [])
		(runIdentity (E.run_ (E.enumList 2 ['A'..'E'] $$ do
			x <- E.last
			extra <- EL.consume
			return (x, extra))))

test_Length :: Suite
test_Length = assertions "length" $ do
	$expect $ equal
		5
		(runIdentity (E.run_ (E.enumList 2 ['A'..'E'] $$ E.length)))

test_LiftTrans :: Suite
test_LiftTrans = assertions "liftTrans" $ do
	let iter1 :: E.Iteratee Char Identity (Maybe Char)
	    iter1 = EL.head
	
	let iter2 :: Bool -> E.Iteratee Char (ReaderT Int Identity) (Maybe Char, [Char])
	    iter2 bad = do
	    	x <- E.liftTrans (if bad then E.throwError (ErrorCall "failed") else iter1)
	    	xs <- EL.consume
	    	return (x, xs)
	
	$expect $ equal
		(Just 'A', ['B', 'C', 'D', 'E'])
		(runIdentity (runReaderT (E.run_ (E.enumList 1 ['A'..'E'] $$ iter2 False)) 0))
	
	$expect $ excEqual
		(ErrorCall "failed")
		(runIdentity (runReaderT (E.run (E.enumList 1 ['A'..'E'] $$ iter2 True)) 0))

test_Peek :: Suite
test_Peek = assertions "peek" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(runIdentity (E.run_ (E.enumList 1 [] $$ do
			x <- E.peek
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(Just 'A', ['A', 'B', 'C', 'D', 'E'])
		(runIdentity (E.run_ (E.enumList 2 ['A'..'E'] $$ do
			x <- E.peek
			extra <- EL.consume
			return (x, extra))))
	$expect $ equal
		(Just 'A', ['A'])
		(runIdentity (E.run_ (E.enumList 1 ['A'] $$ do
			x <- E.peek
			extra <- EL.consume
			return (x, extra))))

test_TryIO :: Suite
test_TryIO = assertions "tryIO" $ do
	do
		res <- E.run (E.tryIO (return 'A'))
		$expect (right res)
		let Right res' = res
		$expect (equal 'A' res')
	
	do
		res <- E.run (E.tryIO (throwIO (ErrorCall "failed")))
		$expect (excEqual (ErrorCall "failed") res)

test_PrintChunks :: Suite
#ifdef MIN_VERSION_silently
test_PrintChunks = assertions "printChunks" $ do
	(stdout, _) <- liftIO (capture (E.run_ (E.enumList 2 ['A', 'B', 'C'] $$ E.printChunks False)))
	$expect (equal stdout "\"AB\"\n\"C\"\nEOF\n")
#else
test_PrintChunks = skipIf True (assertions "printChunks" (return ()))
#endif

excEqual :: (Exception exc, Eq exc) => exc -> Either SomeException b -> Bool
excEqual _ (Right _) = False
excEqual exc1 (Left exc2) = fromException exc2 == Just exc1
