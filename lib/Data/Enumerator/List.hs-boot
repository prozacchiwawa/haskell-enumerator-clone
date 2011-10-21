module Data.Enumerator.List where
import Data.Enumerator.Internal
head :: Monad m => Iteratee a m (Maybe a)
drop :: Monad m => Integer -> Iteratee a m ()
dropWhile :: Monad m => (a -> Bool) -> Iteratee a m ()
takeWhile :: Monad m => (a -> Bool) -> Iteratee a m [a]
consume :: Monad m => Iteratee a m [a]
fold :: Monad m => (b -> a -> b) -> b -> Iteratee a m b
foldM :: Monad m => (b -> a -> m b) -> b -> Iteratee a m b
iterate :: Monad m => (a -> a) -> a -> Enumerator a m b
iterateM :: Monad m => (a -> m a) -> a -> Enumerator a m b
repeat :: Monad m => a -> Enumerator a m b
repeatM :: Monad m => m a -> Enumerator a m b
replicateM :: Monad m => Integer -> m a -> Enumerator a m b
replicate :: Monad m => Integer -> a -> Enumerator a m b
generateM :: Monad m => m (Maybe a) -> Enumerator a m b
map :: Monad m => (ao -> ai) -> Enumeratee ao ai m b
mapM :: Monad m => (ao -> m ai) -> Enumeratee ao ai m b
concatMap :: Monad m => (ao -> [ai]) -> Enumeratee ao ai m b
concatMapM :: Monad m => (ao -> m [ai]) -> Enumeratee ao ai m b
filter :: Monad m => (a -> Bool) -> Enumeratee a a m b
filterM :: Monad m => (a -> m Bool) -> Enumeratee a a m b
