-- |
-- Module: Data.Enumerator.List
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Data.Enumerator.List as EL
-- @
--
-- Since: 0.4.5
module Data.Enumerator.List
	(
	
	-- * List analogues
	
	-- ** Folds
	  fold
	, foldM
	
	-- ** Maps
	, Data.Enumerator.List.map
	, Data.Enumerator.List.mapM
	, Data.Enumerator.List.mapM_
	, Data.Enumerator.List.concatMap
	, concatMapM
	
	-- ** Accumulating maps
	, mapAccum
	, mapAccumM
	, concatMapAccum
	, concatMapAccumM
	
	-- ** Infinite streams
	, Data.Enumerator.List.iterate
	, iterateM
	, Data.Enumerator.List.repeat
	, repeatM
	
	-- ** Bounded streams
	, Data.Enumerator.List.replicate
	, replicateM
	, generateM
	, unfold
	, unfoldM
	
	-- ** Filters
	, Data.Enumerator.List.filter
	, filterM
	
	-- ** Consumers
	, Data.Enumerator.List.take
	, takeWhile
	, consume
	
	-- ** Unsorted
	, head
	, drop
	, Data.Enumerator.List.dropWhile
	, require
	, isolate
	, splitWhen
	, unique
	
	) where

import           Prelude hiding (head, drop, sequence, takeWhile)
import           Control.Exception (ErrorCall(..))
import qualified Control.Monad as CM
import           Control.Monad.Trans.Class (lift)
import qualified Data.List as L
import qualified Data.Set

import           Data.Enumerator hiding ( concatMapM, iterateM, replicateM, head, drop
                                        , foldM, repeatM, generateM, filterM, consume)

-- | Consume the entire input stream with a strict left fold, one element
-- at a time.
--
-- Since: 0.4.8
fold :: Monad m => (b -> a -> b) -> b
       -> Iteratee a m b
fold step = continue . loop where
	f = L.foldl' step
	loop acc stream = case stream of
		Chunks [] -> continue (loop acc)
		Chunks xs -> continue (loop $! f acc xs)
		EOF -> yield acc EOF

-- | Consume the entire input stream with a strict monadic left fold, one
-- element at a time.
--
-- Since: 0.4.8
foldM :: Monad m => (b -> a -> m b) -> b
      -> Iteratee a m b
foldM step = continue . loop where
	f = CM.foldM step
	
	loop acc stream = acc `seq` case stream of
		Chunks [] -> continue (loop acc)
		Chunks xs -> lift (f acc xs) >>= continue . loop
		EOF -> yield acc EOF

-- | Enumerates a stream of elements by repeatedly applying a function to
-- some state.
--
-- Similar to 'Data.Enumerator.List.iterate'.
--
-- Since: 0.4.8
unfold :: Monad m => (s -> Maybe (a, s)) -> s -> Enumerator a m b
unfold f = checkContinue1 $ \loop s k -> case f s of
	Nothing -> continue k
	Just (a, s') -> k (Chunks [a]) >>== loop s'

-- | Enumerates a stream of elements by repeatedly applying a computation to
-- some state.
--
-- Similar to 'iterateM'.
--
-- Since: 0.4.8
unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Enumerator a m b
unfoldM f = checkContinue1 $ \loop s k -> do
	fs <- lift (f s)
	case fs of
		Nothing -> continue k
		Just (a, s') -> k (Chunks [a]) >>== loop s'

-- | @'concatMapM' f@ applies /f/ to each input element and feeds the
-- resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
concatMapM :: Monad m => (ao -> m [ai])
           -> Enumeratee ao ai m b
concatMapM f = checkDone (continue . step) where
	step k EOF = yield (Continue k) EOF
	step k (Chunks xs) = loop k xs
	
	loop k [] = continue (step k)
	loop k (x:xs) = do
		fx <- lift (f x)
		k (Chunks fx) >>==
			checkDoneEx (Chunks xs) (\k' -> loop k' xs)

-- | @'Data.Enumerator.List.concatMap' f@ applies /f/ to each input element
-- and feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
concatMap :: Monad m => (ao -> [ai])
          -> Enumeratee ao ai m b
concatMap f = concatMapM (return . f)

-- | @'Data.Enumerator.List.map' f@ applies /f/ to each input element and
-- feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
map :: Monad m => (ao -> ai)
    -> Enumeratee ao ai m b
map f = Data.Enumerator.List.concatMap (\x -> [f x])

-- | @'Data.Enumerator.List.mapM' f@ applies /f/ to each input element and
-- feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
mapM :: Monad m => (ao -> m ai)
     -> Enumeratee ao ai m b
mapM f = concatMapM (\x -> Prelude.mapM f [x])

-- | @'Data.Enumerator.List.mapM_' f@ applies /f/ to each input element, and
-- discards the results.
--
-- Since: 0.4.11
mapM_ :: Monad m => (a -> m b) -> Iteratee a m ()
mapM_ f = foldM (\_ x -> f x >> return ()) ()

-- | Similar to 'Data.Enumerator.List.concatMap', but with a stateful step
-- function.
--
-- Since: 0.4.11
concatMapAccum :: Monad m => (s -> ao -> (s, [ai])) -> s -> Enumeratee ao ai m b
concatMapAccum f s0 = checkDone (continue . step s0) where
	step _ k EOF = yield (Continue k) EOF
	step s k (Chunks xs) = loop s k xs
	
	loop s k [] = continue (step s k)
	loop s k (x:xs) = case f s x of
		(s', ai) -> k (Chunks ai) >>==
			checkDoneEx (Chunks xs) (\k' -> loop s' k' xs)

-- | Similar to 'concatMapM', but with a stateful step function.
--
-- Since: 0.4.11
concatMapAccumM :: Monad m => (s -> ao -> m (s, [ai])) -> s -> Enumeratee ao ai m b
concatMapAccumM f s0 = checkDone (continue . step s0) where
	step _ k EOF = yield (Continue k) EOF
	step s k (Chunks xs) = loop s k xs
	
	loop s k [] = continue (step s k)
	loop s k (x:xs) = do
		(s', ai) <- lift (f s x)
		k (Chunks ai) >>==
			checkDoneEx (Chunks xs) (\k' -> loop s' k' xs)

-- | Similar to 'Data.Enumerator.List.map', but with a stateful step function.
--
-- Since: 0.4.9
mapAccum :: Monad m => (s -> ao -> (s, ai)) -> s -> Enumeratee ao ai m b
mapAccum f = concatMapAccum (\s ao -> case f s ao of (s', ai) -> (s', [ai]))

-- | Similar to 'Data.Enumerator.List.mapM', but with a stateful step function.
--
-- Since: 0.4.9
mapAccumM :: Monad m => (s -> ao -> m (s, ai)) -> s -> Enumeratee ao ai m b
mapAccumM f = concatMapAccumM (\s ao -> do
	(s', ai) <- f s ao
	return (s', [ai]))

-- | @'Data.Enumerator.List.iterate' f x@ enumerates an infinite stream of
-- repeated applications of /f/ to /x/.
--
-- Analogous to 'Prelude.iterate'.
--
-- Since: 0.4.8
iterate :: Monad m => (a -> a) -> a -> Enumerator a m b
iterate f = checkContinue1 $ \loop s k -> k (Chunks [s]) >>== loop (f s)

-- | Similar to 'Data.Enumerator.List.iterate', except the iteration
-- function is monadic.
--
-- Since: 0.4.8
iterateM :: Monad m => (a -> m a) -> a
         -> Enumerator a m b
iterateM f base = worker (return base) where
	worker = checkContinue1 $ \loop m_a k -> do
		a <- lift m_a
		k (Chunks [a]) >>== loop (f a)

-- | Enumerates an infinite stream of a single element.
--
-- Analogous to 'Prelude.repeat'.
--
-- Since: 0.4.8
repeat :: Monad m => a -> Enumerator a m b
repeat a = checkContinue0 $ \loop k -> k (Chunks [a]) >>== loop

-- | Enumerates an infinite stream of element. Each element is computed by
-- the underlying monad.
--
-- Since: 0.4.8
repeatM :: Monad m => m a -> Enumerator a m b
repeatM m_a step = do
	a <- lift m_a
	iterateM (const m_a) a step

-- | @'replicateM' n m_x@ enumerates a stream of /n/ elements, with each
-- element computed by /m_x/.
--
-- Since: 0.4.8
replicateM :: Monad m => Integer -> m a
           -> Enumerator a m b
replicateM maxCount getNext = loop maxCount where
	loop 0 step = returnI step
	loop n (Continue k) = do
		next <- lift getNext
		k (Chunks [next]) >>== loop (n - 1)
	loop _ step = returnI step

-- | @'Data.Enumerator.List.replicate' n x@ enumerates a stream containing
-- /n/ copies of /x/.
--
-- Analogous to 'Prelude.replicate'.
--
-- Since: 0.4.8
replicate :: Monad m => Integer -> a
          -> Enumerator a m b
replicate maxCount a = replicateM maxCount (return a)

-- | Like 'repeatM', except the computation may terminate the stream by
-- returning 'Nothing'.
--
-- Since: 0.4.8
generateM :: Monad m => m (Maybe a)
          -> Enumerator a m b
generateM getNext = checkContinue0 $ \loop k -> do
	next <- lift getNext
	case next of
		Nothing -> continue k
		Just x -> k (Chunks [x]) >>== loop

-- | Applies a predicate to the stream. The inner iteratee only receives
-- elements for which the predicate is @True@.
--
-- Since: 0.4.8
filter :: Monad m => (a -> Bool)
       -> Enumeratee a a m b
filter p = Data.Enumerator.List.concatMap (\x -> [x | p x])

-- | Applies a monadic predicate to the stream. The inner iteratee only
-- receives elements for which the predicate returns @True@.
--
-- Since: 0.4.8
filterM :: Monad m => (a -> m Bool)
        -> Enumeratee a a m b
filterM p = concatMapM (\x -> CM.filterM p [x])

-- | @'Data.Enumerator.List.take' n@ extracts the next /n/ elements from the
-- stream, as a list.
--
-- Since: 0.4.5
take :: Monad m => Integer -> Iteratee a m [a]
take n | n <= 0 = return []
take n = continue (loop id n) where
	len = L.genericLength
	loop acc n' (Chunks xs)
		| len xs < n' = continue (loop (acc . (xs ++)) (n' - len xs))
		| otherwise   = let
			(xs', extra) = L.genericSplitAt n' xs
			in yield (acc xs') (Chunks extra)
	loop acc _ EOF = yield (acc []) EOF

-- | @'takeWhile' p@ extracts input from the stream until the first element
-- which does not match the predicate.
--
-- Since: 0.4.5
takeWhile :: Monad m => (a -> Bool) -> Iteratee a m [a]
takeWhile p = continue (loop id) where
	loop acc (Chunks []) = continue (loop acc)
	loop acc (Chunks xs) = case Prelude.span p xs of
		(_, []) -> continue (loop (acc . (xs ++)))
		(xs', extra) -> yield (acc xs') (Chunks extra)
	loop acc EOF = yield (acc []) EOF

-- | @'consume' = 'takeWhile' (const True)@
--
-- Since: 0.4.5
consume :: Monad m => Iteratee a m [a]
consume = continue (loop id) where
	loop acc (Chunks []) = continue (loop acc)
	loop acc (Chunks xs) = continue (loop (acc . (xs ++)))
	loop acc EOF = yield (acc []) EOF

-- | Get the next element from the stream, or 'Nothing' if the stream has
-- ended.
--
-- Since: 0.4.5
head :: Monad m => Iteratee a m (Maybe a)
head = continue loop where
	loop (Chunks []) = head
	loop (Chunks (x:xs)) = yield (Just x) (Chunks xs)
	loop EOF = yield Nothing EOF

-- | @'drop' n@ ignores /n/ input elements from the stream.
--
-- Since: 0.4.5
drop :: Monad m => Integer -> Iteratee a m ()
drop n | n <= 0 = return ()
drop n = continue (loop n) where
	loop n' (Chunks xs) = iter where
		len = L.genericLength xs
		iter = if len < n'
			then drop (n' - len)
			else yield () (Chunks (L.genericDrop n' xs))
	loop _ EOF = yield () EOF

-- | @'Data.Enumerator.List.dropWhile' p@ ignores input from the stream
-- until the first element which does not match the predicate.
--
-- Since: 0.4.5
dropWhile :: Monad m => (a -> Bool) -> Iteratee a m ()
dropWhile p = continue loop where
	loop (Chunks xs) = case L.dropWhile p xs of
		[] -> continue loop
		xs' -> yield () (Chunks xs')
	loop EOF = yield () EOF

-- | @'require' n@ buffers input until at least /n/ elements are available, or
-- throws an error if the stream ends early.
--
-- Since: 0.4.5
require :: Monad m => Integer -> Iteratee a m ()
require n | n <= 0 = return ()
require n = continue (loop id n) where
	len = L.genericLength
	loop acc n' (Chunks xs)
		| len xs < n' = continue (loop (acc . (xs ++)) (n' - len xs))
		| otherwise   = yield () (Chunks (acc xs))
	loop _ _ EOF = throwError (ErrorCall "require: Unexpected EOF")

-- | @'isolate' n@ reads at most /n/ elements from the stream, and passes them
-- to its iteratee. If the iteratee finishes early, elements continue to be
-- consumed from the outer stream until /n/ have been consumed.
--
-- Since: 0.4.5
isolate :: Monad m => Integer -> Enumeratee a a m b
isolate n step | n <= 0 = return step
isolate n (Continue k) = continue loop where
	len = L.genericLength
	
	loop (Chunks []) = continue loop
	loop (Chunks xs)
		| len xs <= n = k (Chunks xs) >>== isolate (n - len xs)
		| otherwise = let
			(s1, s2) = L.genericSplitAt n xs
			in k (Chunks s1) >>== (\step -> yield step (Chunks s2))
	loop EOF = k EOF >>== (\step -> yield step EOF)
isolate n step = drop n >> return step

-- | Split on elements satisfying a given predicate.
--
-- Since: 0.4.8
splitWhen :: Monad m => (a -> Bool) -> Enumeratee a [a] m b
splitWhen p = sequence $ do
	as <- takeWhile (not . p)
	drop 1
	return as

-- | Remove duplicate elements from a stream, passing through the first
-- instance of each value.
--
-- Similar to 'nub', but more efficient because it uses a 'Data.Set.Set'
-- internally.
--
-- Since: 0.4.11
unique :: (Ord a, Monad m) => Enumeratee a a m b
unique = concatMapAccum step Data.Set.empty where
	step s x = if Data.Set.member x s
		then (s, [])
		else (Data.Set.insert x s, [x])
