{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Compatibility
	( test_Compatibility
	) where

import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Text (Text)
import           Test.Chell

import qualified Data.Enumerator as E
import           Data.Enumerator (($$), (=$))
import qualified Data.Enumerator.List as EL

compatIter :: (Eq a, Show a)
           => Text
           -> E.Iteratee Char Identity a
           -> E.Iteratee Char Identity a
           -> Suite
compatIter name i1 i2 = assertions name $ do
	let run i = runIdentity (E.run_ (E.enumList 1 "ABCDE" $$ do
		x <- i
		y <- EL.consume
		return (x, y)))
	$expect (equal (run i1) (run i2))

compatEnum :: (Eq a, Show a)
           => Text
           -> E.Enumerator a Identity [a]
           -> E.Enumerator a Identity [a]
           -> Suite
compatEnum name e1 e2 = assertions name $ do
	let run e = runIdentity (E.run_ (e $$ EL.take 10))
	$expect (equal (run e1) (run e2))

compatEnee :: (Eq ai, Show ai)
           => Text
           -> E.Enumeratee Char ai Identity [ai]
           -> E.Enumeratee Char ai Identity [ai]
           -> Suite
compatEnee name e1 e2 = assertions name $ do
	let run e = runIdentity (E.run_ (E.enumList 1 ['A'..'Z'] $$ e =$ EL.consume))
	$expect (equal (run e1) (run e2))

$([d||])

test_Compatibility :: Suite
test_Compatibility = suite "compatibility"
	[ test_Head
	, test_Drop
	, test_DropWhile
	, test_Span
	, test_Break
	, test_Consume
	, test_Foldl
	, test_Foldl'
	, test_FoldM
	, test_Iterate
	, test_IterateM
	, test_Repeat
	, test_RepeatM
	, test_Replicate
	, test_ReplicateM
	, test_GenerateM
	, test_Map
	, test_MapM
	, test_ConcatMap
	, test_ConcatMapM
	, test_Filter
	, test_FilterM
	, test_LiftFoldL
	, test_LiftFoldL'
	, test_LiftFoldM
	]

test_Head :: Suite
test_Head = compatIter "head" E.head EL.head

test_Drop :: Suite
test_Drop = compatIter "drop" (E.drop 1) (EL.drop 1)

test_DropWhile :: Suite
test_DropWhile = compatIter "dropWhile"
	(E.dropWhile (< 'C'))
	(EL.dropWhile (< 'C'))

test_Span :: Suite
test_Span = compatIter "span"
	(E.span (< 'C'))
	(EL.takeWhile (< 'C'))

test_Break :: Suite
test_Break = compatIter "break"
	(E.break (> 'C'))
	(EL.takeWhile (<= 'C'))

test_Consume :: Suite
test_Consume = compatIter "consume" E.consume EL.consume

test_Foldl :: Suite
test_Foldl = compatIter "foldl"
	(E.foldl (flip (:)) [])
	(EL.fold (flip (:)) [])

test_LiftFoldL :: Suite
test_LiftFoldL = compatIter "liftFoldL"
	(E.liftFoldL (flip (:)) [])
	(EL.fold (flip (:)) [])

test_Foldl' :: Suite
test_Foldl' = compatIter "foldl'"
	(E.foldl' (flip (:)) [])
	(EL.fold (flip (:)) [])

test_LiftFoldL' :: Suite
test_LiftFoldL' = compatIter "liftFoldl'"
	(E.liftFoldL' (flip (:)) [])
	(EL.fold (flip (:)) [])

test_FoldM :: Suite
test_FoldM = compatIter "foldM"
	(E.foldM (\xs x -> return (x:xs)) [])
	(EL.foldM (\xs x -> return (x:xs)) [])

test_LiftFoldM :: Suite
test_LiftFoldM = compatIter "liftFoldM"
	(E.liftFoldM (\xs x -> return (x:xs)) [])
	(EL.foldM (\xs x -> return (x:xs)) [])

test_Iterate :: Suite
test_Iterate = compatEnum "iterate"
	(E.iterate succ 'A')
	(EL.iterate succ 'A')

test_IterateM :: Suite
test_IterateM = compatEnum "iterateM"
	(E.iterateM (return . succ) 'A')
	(EL.iterateM (return . succ) 'A')

test_Repeat :: Suite
test_Repeat = compatEnum "repeat"
	(E.repeat 'A')
	(EL.repeat 'A')

test_RepeatM :: Suite
test_RepeatM = compatEnum "repeatM"
	(E.repeatM (return 'A'))
	(EL.repeatM (return 'A'))

test_Replicate :: Suite
test_Replicate = compatEnum "replicate"
	(E.replicate 5 'A')
	(EL.replicate 5 'A')

test_ReplicateM :: Suite
test_ReplicateM = compatEnum "replicateM"
	(E.replicateM 5 (return 'A'))
	(EL.replicateM 5 (return 'A'))

test_GenerateM :: Suite
test_GenerateM = compatEnum "generateM"
	(E.generateM (return (Just 'A')))
	(EL.generateM (return (Just 'A')))

test_Map :: Suite
test_Map = compatEnee "map"
	(E.map succ)
	(EL.map succ)

test_MapM :: Suite
test_MapM = compatEnee "mapM"
	(E.mapM (return . succ))
	(EL.mapM (return . succ))

test_ConcatMap :: Suite
test_ConcatMap = compatEnee "concatMap"
	(E.concatMap (\x -> [succ x]))
	(EL.concatMap (\x -> [succ x]))

test_ConcatMapM :: Suite
test_ConcatMapM = compatEnee "concatMapM"
	(E.concatMapM (\x -> return [succ x]))
	(EL.concatMapM (\x -> return [succ x]))

test_Filter :: Suite
test_Filter = compatEnee "filter"
	(E.filter (< 'C'))
	(EL.filter (< 'C'))

test_FilterM :: Suite
test_FilterM = compatEnee "filterM"
	(E.filterM (return . (< 'C')))
	(EL.filterM (return . (< 'C')))
