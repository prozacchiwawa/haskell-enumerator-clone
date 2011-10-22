{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Misc
	( test_ConcatEnums
	, test_EnumEOF
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

import           Data.Enumerator (($$), (<==<))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util (equalExc)

test_ConcatEnums :: Suite
test_ConcatEnums = assertions "concatEnums" $ do
	let enum = E.concatEnums
		[ E.enumLists [['A']]
		, E.enumLists [['B']]
		, E.enumLists [['C']]
		]
	$expect $ equal
		['A', 'B', 'C']
		(runIdentity (E.run_ (enum $$ EL.consume)))
	$expect $ equal
		['A', 'B']
		(runIdentity (E.run_ (E.enumLists [['B']] <==< E.enumLists [['A']] $$ EL.consume)))

test_EnumEOF :: Suite
test_EnumEOF = assertions "enumEOF" $ do
	let iter = E.continue (\_ -> iter)
	
	$expect $ throwsEq
		(ErrorCall "enumEOF: divergent iteratee")
		(E.runIteratee (E.enumEOF $$ iter))

test_Last :: Suite
test_Last = assertions "last" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(E.runLists_ [[]] $ do
			x <- E.last
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 'E', [])
		(E.runLists_ [['A', 'B'], ['C', 'D'], ['E']] $ do
			x <- E.last
			extra <- EL.consume
			return (x, extra))

test_Length :: Suite
test_Length = assertions "length" $ do
	$expect $ equal
		5
		(E.runLists_ [['A', 'B'], ['C', 'D'], ['E']] E.length)

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
	
	$expect $ equalExc
		(ErrorCall "failed")
		(runIdentity (runReaderT (E.run (E.enumList 1 ['A'..'E'] $$ iter2 True)) 0))

test_Peek :: Suite
test_Peek = assertions "peek" $ do
	$expect $ equal
		(Nothing :: Maybe Char, [])
		(E.runLists_ [] $ do
			x <- E.peek
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 'A', ['A', 'B', 'C', 'D', 'E'])
		(E.runLists_ [[], ['A', 'B'], ['C', 'D'], ['E']] $ do
			x <- E.peek
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Just 'A', ['A'])
		(E.runLists_ [['A']] $ do
			x <- E.peek
			extra <- EL.consume
			return (x, extra))

test_TryIO :: Suite
test_TryIO = assertions "tryIO" $ do
	do
		res <- E.run (E.tryIO (return 'A'))
		$expect (right res)
		let Right res' = res
		$expect (equal 'A' res')
	
	do
		res <- E.run (E.tryIO (throwIO (ErrorCall "failed")))
		$expect (equalExc (ErrorCall "failed") res)

test_PrintChunks :: Suite
#ifdef MIN_VERSION_silently
test_PrintChunks = assertions "printChunks" $ do
	do
		(stdout, _) <- liftIO (capture (E.run_ (E.enumLists [[], ['A', 'B'], ['C']] $$ E.printChunks False)))
		$expect (equal stdout "\"AB\"\n\"C\"\nEOF\n")
	do
		(stdout, _) <- liftIO (capture (E.run_ (E.enumLists [[], ['A', 'B'], ['C']] $$ E.printChunks True)))
		$expect (equal stdout "\"\"\n\"AB\"\n\"C\"\nEOF\n")
#else
test_PrintChunks = skipIf True (assertions "printChunks" (return ()))
#endif
