{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.CatchError
	( test_CatchError
	) where

import           Control.Concurrent
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class (liftIO)

import           Test.Chell

import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import           EnumeratorTests.Util (within)

test_CatchError :: Suite
test_CatchError = suite "catchError"
	[ test_WithoutContinue
	, test_NotDivergent
	, test_Interleaved
	, test_YieldImmediately
	, test_HandleError
	, test_HandleEOF
	, test_GotStream
	]

test_WithoutContinue :: Suite
test_WithoutContinue = assertions "without-continue" $ do
	let iter = E.catchError
	    	(E.throwError (Exc.ErrorCall "error"))
	    	(\_ -> EL.require 1)
	
	res <- E.run (E.enumList 1 [] $$ iter)
	$assert (left res)
	
	let Left err = res
	$assert $ equal (Exc.fromException err) (Just (Exc.ErrorCall "require: Unexpected EOF"))

test_NotDivergent :: Suite
test_NotDivergent = assertions "not-divergent" $ do
	let iter = E.catchError
	    	(do
	    		_ <- EL.head
	    		E.throwError (Exc.ErrorCall "error"))
	    	(\_ -> EL.require 1)
	
	res <- E.run (E.enumList 1 [] $$ iter)
	$assert (left res)
	
	let Left err = res
	$assert $ equal (Exc.fromException err) (Just (Exc.ErrorCall "require: Unexpected EOF"))

test_Interleaved :: Suite
test_Interleaved = within 1000 $ assertions "interleaved" $ do
	let enumMVar mvar = EL.repeatM (liftIO (takeMVar mvar))
	let iter mvar = do
	    	liftIO (putMVar mvar ())
	    	_ <- EL.head
	    	return True
	let onError _ = return False
	
	mvar <- liftIO newEmptyMVar
	E.run_ (enumMVar mvar $$ E.catchError (iter mvar) onError)

test_YieldImmediately :: Suite
test_YieldImmediately = assertions "yield-immediately" $ do
	res <- E.run_ (E.enumList 1 ['A'] $$ E.catchError (return 'A') (\_ -> return 'B'))
	$expect $ equal 'A' res

test_HandleError :: Suite
test_HandleError = assertions "handle-error" $ do
	do
		res <- E.run_ (E.enumList 1 [] $$ E.catchError
			(EL.head >> E.throwError (Exc.ErrorCall "error"))
			(\err -> return (show err)))
		$expect $ equal "error" res
	do
		res <- E.run_ (E.enumList 1 ['A'] $$ E.catchError
			(E.throwError (Exc.ErrorCall "error"))
			(\err -> return (show err)))
		$expect $ equal "error" res
	do
		res <- E.run_ (E.enumList 1 ['A', 'B', 'C'] $$ E.catchError
			(EL.drop 1 >> E.throwError (Exc.ErrorCall "error"))
			(\err -> return (show err)))
		$expect $ equal "error" res

test_HandleEOF :: Suite
test_HandleEOF = assertions "handle-eof" $ do
	res <- E.run_ (E.enumList 1 [] $$ E.catchError EL.head (\_ -> return (Just 'B')))
	$expect $ equal Nothing res

test_GotStream :: Suite
test_GotStream = assertions "got-stream" $ do
	let iter = E.catchError
	    	(do
	    		_ <- EL.head
	    		_ <- EL.head
	    		E.throwError (Exc.ErrorCall "error"))
	    	(\_ -> EL.head)
	
	res <- E.run_ (E.enumList 1 ['A', 'B', 'C'] $$ iter)
	$assert $ equal (Just 'B') res
