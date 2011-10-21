-- |
-- Module: Data.Enumerator.Compatibility
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Shims for compatibility with earlier versions of the Enumerator library.
module Data.Enumerator.Compatibility
	( liftI
	, head
	, drop
	, dropWhile
	, span
	, break
	, consume
	, foldl
	, foldl'
	, foldM
	, iterate
	, iterateM
	, repeat
	, repeatM
	, replicate
	, replicateM
	, generateM
	, map
	, mapM
	, concatMap
	, concatMapM
	, filter
	, filterM
	, liftFoldL
	, liftFoldL'
	, liftFoldM
	
	) where

import qualified Prelude
import           Prelude (Bool, Integer, Maybe, Monad, not, (.))

import           Data.Enumerator.Internal
import {-# SOURCE #-} qualified Data.Enumerator.List as EL

{-# DEPRECATED liftI "Use 'Data.Enumerator.continue' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.continue' instead
liftI :: Monad m => (Stream a -> Step a m b)
      -> Iteratee a m b
liftI k = continue (returnI . k)

{-# DEPRECATED head "Use 'Data.Enumerator.List.head' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.head' instead
head :: Monad m => Iteratee a m (Maybe a)
head = EL.head

{-# DEPRECATED drop "Use 'Data.Enumerator.List.drop' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.drop' instead
drop :: Monad m => Integer -> Iteratee a m ()
drop = EL.drop

{-# DEPRECATED dropWhile "Use 'Data.Enumerator.List.dropWhile' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.dropWhile' instead
dropWhile :: Monad m => (a -> Bool) -> Iteratee a m ()
dropWhile = EL.dropWhile

{-# DEPRECATED span "Use 'Data.Enumerator.List.takeWhile' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.takeWhile' instead
span :: Monad m => (a -> Bool) -> Iteratee a m [a]
span = EL.takeWhile

{-# DEPRECATED break "Use 'Data.Enumerator.List.takeWhile' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.takeWhile' instead
break :: Monad m => (a -> Bool) -> Iteratee a m [a]
break p = EL.takeWhile (not . p)

{-# DEPRECATED consume "Use 'Data.Enumerator.List.consume' instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.consume' instead
consume :: Monad m => Iteratee a m [a]
consume = EL.consume

{-# DEPRECATED foldl "Use Data.Enumerator.List.fold instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.fold' instead
--
-- Since: 0.4.5
foldl :: Monad m => (b -> a -> b) -> b -> Iteratee a m b
foldl step = continue . loop where
	fold = Prelude.foldl step
	loop acc stream = case stream of
		Chunks [] -> continue (loop acc)
		Chunks xs -> continue (loop (fold acc xs))
		EOF -> yield acc EOF

{-# DEPRECATED foldl' "Use Data.Enumerator.List.fold instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.fold' instead
--
-- Since: 0.4.5
foldl' :: Monad m => (b -> a -> b) -> b -> Iteratee a m b
foldl' = EL.fold

{-# DEPRECATED foldM "Use Data.Enumerator.List.foldM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.foldM' instead
--
-- Since: 0.4.5
foldM :: Monad m => (b -> a -> m b) -> b -> Iteratee a m b
foldM = EL.foldM

{-# DEPRECATED iterate "Use Data.Enumerator.List.iterate instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.iterate' instead
--
-- Since: 0.4.5
iterate :: Monad m => (a -> a) -> a -> Enumerator a m b
iterate = EL.iterate

{-# DEPRECATED iterateM "Use Data.Enumerator.List.iterateM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.iterateM' instead
--
-- Since: 0.4.5
iterateM :: Monad m => (a -> m a) -> a -> Enumerator a m b
iterateM = EL.iterateM

{-# DEPRECATED repeat "Use Data.Enumerator.List.repeat instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.repeat' instead
--
-- Since: 0.4.5
repeat :: Monad m => a -> Enumerator a m b
repeat = EL.repeat

{-# DEPRECATED repeatM "Use Data.Enumerator.List.repeatM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.repeatM' instead
--
-- Since: 0.4.5
repeatM :: Monad m => m a -> Enumerator a m b
repeatM = EL.repeatM

{-# DEPRECATED replicate "Use Data.Enumerator.List.replicate instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.replicate' instead
--
-- Since: 0.4.5
replicate :: Monad m => Integer -> a -> Enumerator a m b
replicate = EL.replicate

{-# DEPRECATED replicateM "Use Data.Enumerator.List.replicateM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.replicateM' instead
--
-- Since: 0.4.5
replicateM :: Monad m => Integer -> m a -> Enumerator a m b
replicateM = EL.replicateM

{-# DEPRECATED generateM "Use Data.Enumerator.List.generateM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.generateM' instead
--
-- Since: 0.4.5
generateM :: Monad m => m (Maybe a) -> Enumerator a m b
generateM = EL.generateM

{-# DEPRECATED map "Use Data.Enumerator.List.map instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.map' instead
map :: Monad m => (ao -> ai) -> Enumeratee ao ai m b
map = EL.map

{-# DEPRECATED mapM "Use Data.Enumerator.List.mapM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.mapM' instead
--
-- Since: 0.4.3
mapM :: Monad m => (ao -> m ai) -> Enumeratee ao ai m b
mapM = EL.mapM

{-# DEPRECATED concatMap "Use Data.Enumerator.List.concatMap instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.concatMap' instead
--
-- Since: 0.4.3
concatMap :: Monad m => (ao -> [ai]) -> Enumeratee ao ai m b
concatMap = EL.concatMap

{-# DEPRECATED concatMapM "Use Data.Enumerator.List.concatMapM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.concatMapM' instead
--
-- Since: 0.4.5
concatMapM :: Monad m => (ao -> m [ai]) -> Enumeratee ao ai m b
concatMapM = EL.concatMapM

{-# DEPRECATED filter "Use Data.Enumerator.List.filter instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.filter' instead
--
-- Since: 0.4.5
filter :: Monad m => (a -> Bool) -> Enumeratee a a m b
filter = EL.filter

{-# DEPRECATED filterM "Use Data.Enumerator.List.filterM instead" #-}
-- | Deprecated in 0.4.8: use 'Data.Enumerator.List.filterM' instead
--
-- Since: 0.4.5
filterM :: Monad m => (a -> m Bool) -> Enumeratee a a m b
filterM = EL.filterM

{-# DEPRECATED liftFoldL "Use Data.Enumerator.List.fold instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.fold' instead
--
-- Since: 0.1.1
liftFoldL :: Monad m => (b -> a -> b) -> b
          -> Iteratee a m b
liftFoldL = foldl

{-# DEPRECATED liftFoldL' "Use Data.Enumerator.List.fold instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.fold' instead
--
-- Since: 0.1.1
liftFoldL' :: Monad m => (b -> a -> b) -> b
           -> Iteratee a m b
liftFoldL' = EL.fold

{-# DEPRECATED liftFoldM "Use Data.Enumerator.List.foldM instead" #-}
-- | Deprecated in 0.4.5: use 'Data.Enumerator.List.foldM' instead
--
-- Since: 0.1.1
liftFoldM :: Monad m => (b -> a -> m b) -> b
          -> Iteratee a m b
liftFoldM = EL.foldM
