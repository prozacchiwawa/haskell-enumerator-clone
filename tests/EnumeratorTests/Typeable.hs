{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Typeable
	( test_Typeable
	) where

import           Data.Typeable (typeOf)
import           Test.Chell

import           Data.Enumerator

test_Typeable :: Suite
test_Typeable = suite "instance-typeable"
	[ test_TypeableStream
	, test_TypeableIteratee
	, test_TypeableStep
	]

test_TypeableStream :: Suite
test_TypeableStream = assertions "stream" $ do
	let x = undefined :: Stream Char
	$expect $ equal
		"Data.Enumerator.Stream Char"
		(show (typeOf x))

test_TypeableIteratee :: Suite
test_TypeableIteratee = assertions "iteratee" $ do
	let x = undefined :: Iteratee Char Maybe Int
	$expect $ equal
		"Data.Enumerator.Iteratee Char Maybe Int"
		(show (typeOf x))

test_TypeableStep :: Suite
test_TypeableStep = assertions "step" $ do
	let x = undefined :: Step Char Maybe Int
	$expect $ equal
		"Data.Enumerator.Step Char Maybe Int"
		(show (typeOf x))
