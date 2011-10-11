{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Instances
	( test_Instances
	) where

import           Control.Applicative (pure, (<*>))
import           Data.Functor.Identity (runIdentity)
import           Data.Typeable (typeOf)
import           Test.Chell

import           Data.Enumerator

test_Instances :: Suite
test_Instances = suite "instances"
	[ test_Typeable
	, test_Functor
	, test_Applicative
	]

test_Typeable :: Suite
test_Typeable = suite "typeable"
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

test_Functor :: Suite
test_Functor = assertions "functor" $ do
	$expect $ equal
		'B'
		(runIdentity (run_ (fmap succ (return 'A'))))

test_Applicative :: Suite
test_Applicative = assertions "applicative" $ do
	$expect $ equal
		'A'
		(runIdentity (run_ (pure 'A')))
	$expect $ equal
		'B'
		(runIdentity (run_ (pure succ <*> pure 'A')))
