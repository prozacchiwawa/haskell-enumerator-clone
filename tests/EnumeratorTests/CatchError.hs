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
	[ test_CatchError_WithoutContinue
	, test_CatchError_NotDivergent
	, test_CatchError_Interleaved
	]

test_CatchError_WithoutContinue :: Suite
test_CatchError_WithoutContinue = assertions "without-continue" $ do
	let iter = E.catchError
	    	(E.throwError (Exc.ErrorCall "error"))
	    	(\_ -> EL.require 1)
	
	res <- E.run (E.enumList 1 [] $$ iter)
	$assert (left res)
	
	let Left err = res
	$assert $ equal (Exc.fromException err) (Just (Exc.ErrorCall "require: Unexpected EOF"))

test_CatchError_NotDivergent :: Suite
test_CatchError_NotDivergent = assertions "not-divergent" $ do
	let iter = E.catchError
	    	(do
	    		_ <- EL.head
	    		E.throwError (Exc.ErrorCall "error"))
	    	(\_ -> EL.require 1)
	
	res <- E.run (E.enumList 1 [] $$ iter)
	$assert (left res)
	
	let Left err = res
	$assert $ equal (Exc.fromException err) (Just (Exc.ErrorCall "require: Unexpected EOF"))

test_CatchError_Interleaved :: Suite
test_CatchError_Interleaved = within 1000 $ assertions "interleaved" $ do
	let enumMVar mvar = EL.repeatM (liftIO (takeMVar mvar))
	let iter mvar = do
	    	liftIO (putMVar mvar ())
	    	_ <- EL.head
	    	return True
	let onError _ = return False
	
	mvar <- liftIO newEmptyMVar
	E.run_ (enumMVar mvar $$ E.catchError (iter mvar) onError)
