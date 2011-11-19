-- |
-- Module: Data.Enumerator.Trans
-- Copyright: 2011 Mikhail Vorozhtsov
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- This module provides lifted versions of monad transformer runners.
--
-- Since: 0.4.16
module Data.Enumerator.Trans
	( runIdentityI
	, runMaybeI
	, runErrorI
	, runReaderI
	, runStateI
	, evalStateI
	, runStateI'
	, evalStateI'
	, runWriterI
	, execWriterI
	, runWriterI'
	, execWriterI'
	, runRWSI
	, evalRWSI
	, execRWSI
	, runRWSI'
	, evalRWSI'
	, execRWSI'
	) where

import           Data.Monoid (Monoid(..))
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S

import           Data.Enumerator (Stream(..), Step(..), Iteratee(..))

-- | Lifted version of 'runIdentityT'
--
-- Since: 0.4.16
runIdentityI :: Monad m => Iteratee i (IdentityT m) a -> Iteratee i m a
runIdentityI it = Iteratee $ do
	step <- runIdentityT $ runIteratee it
	return $ case step of
		Continue k -> Continue $ runIdentityI . k
		Yield x cs -> Yield x cs
		Error e    -> Error e

-- | Lifted version of 'runMaybeT'
--
-- Since: 0.4.16
runMaybeI :: Monad m => Iteratee i (MaybeT m) a -> Iteratee i m (Maybe a)
runMaybeI it = Iteratee $ do
	mStep <- runMaybeT $ runIteratee it
	return $ case mStep of
		Nothing   -> Yield Nothing $ Chunks []
		Just step -> case step of
			Continue k -> Continue $ runMaybeI . k
			Yield x cs -> Yield (Just x) cs
			Error e    -> Error e

-- | Lifted version of 'runErrorT'
--
-- Since: 0.4.16
runErrorI :: (Error e, Monad m)
          => Iteratee i (ErrorT e m) a -> Iteratee i m (Either e a)
runErrorI it = Iteratee $ do
	mStep <- runErrorT $ runIteratee it
	return $ case mStep of
		Left e     -> Yield (Left e) $ Chunks []
		Right step -> case step of
			Continue k -> Continue $ runErrorI . k
			Yield x cs -> Yield (Right x) cs
			Error e    -> Error e

-- | Lifted version of 'runReaderT'
--
-- Since: 0.4.16
runReaderI :: Monad m => r -> Iteratee i (ReaderT r m) a -> Iteratee i m a
runReaderI r it = Iteratee $ do
	step <- runReaderT (runIteratee it) r
	return $ case step of
		Continue k -> Continue $ runReaderI r . k
		Yield x cs -> Yield x cs
		Error e    -> Error e

-- | Lifted version of (lazy) 'L.runStateT'
--
-- Since: 0.4.16
runStateI :: Monad m => s -> Iteratee i (L.StateT s m) a -> Iteratee i m (a, s)
runStateI s it = Iteratee $ do
	~(step, s') <- L.runStateT (runIteratee it) s
	return $ case step of
		Continue k -> Continue $ runStateI s' . k
		Yield x cs -> Yield (x, s') cs
		Error e    -> Error e

-- | Lifted version of (lazy) 'L.evalStateT'
--
-- Since: 0.4.16
evalStateI :: Monad m => s -> Iteratee i (L.StateT s m) a -> Iteratee i m a
evalStateI s = fmap fst . runStateI s

-- | Lifted version of (strict) 'S.runStateT'
--
-- Since: 0.4.16
runStateI' :: Monad m => s -> Iteratee i (S.StateT s m) a -> Iteratee i m (a, s)
runStateI' s it = Iteratee $ do
	(step, s') <- S.runStateT (runIteratee it) s
	return $ case step of
		Continue k -> Continue $ runStateI' s' . k
		Yield x cs -> Yield (x, s') cs
		Error e    -> Error e

-- | Lifted version of (strict) 'S.evalStateT'
--
-- Since: 0.4.16
evalStateI' :: Monad m => s -> Iteratee i (S.StateT s m) a -> Iteratee i m a
evalStateI' s = fmap fst . runStateI' s

-- | Lifted version of (lazy) 'L.runWriterT'
--
-- Since: 0.4.16
runWriterI :: (Monoid w, Monad m)
           => Iteratee i (L.WriterT w m) a -> Iteratee i m (a, w)
runWriterI it0 = go mempty it0 where
	go w it = Iteratee $ do
		~(step, w') <- L.runWriterT $ runIteratee it
		return $ case step of
			Continue k -> Continue $ go (w `mappend` w') . k
			Yield x cs -> Yield (x, w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (lazy) 'L.execWriterT'
--
-- Since: 0.4.16
execWriterI :: (Monoid w, Monad m)
            => Iteratee i (L.WriterT w m) a -> Iteratee i m w
execWriterI = fmap snd . runWriterI

-- | Lifted version of (strict) 'S.runWriterT'
--
-- Since: 0.4.16
runWriterI' :: (Monoid w, Monad m)
            => Iteratee i (S.WriterT w m) a -> Iteratee i m (a, w)
runWriterI' it0 = go mempty it0 where
	go w it = Iteratee $ do
		(step, w') <- S.runWriterT $ runIteratee it
		return $ case step of
			Continue k -> Continue $ go (w `mappend` w') . k
			Yield x cs -> Yield (x, w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (strict) 'L.execWriterT'
--
-- Since: 0.4.16
execWriterI' :: (Monoid w, Monad m)
             => Iteratee i (S.WriterT w m) a -> Iteratee i m w
execWriterI' = fmap snd . runWriterI'

-- | Lifted version of (lazy) 'L.runRWST'
--
-- Since: 0.4.16
runRWSI :: (Monoid w, Monad m)
        => r -> s -> Iteratee i (L.RWST r w s m) a -> Iteratee i m (a, s, w)
runRWSI r s0 it0 = go s0 mempty it0 where
	go s w it = Iteratee $ do
		~(step, s', w') <- L.runRWST (runIteratee it) r s
		return $ case step of
			Continue k -> Continue $ go s' (w `mappend` w') . k
			Yield x cs -> Yield (x, s', w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (lazy) 'L.evalRWST'
--
-- Since: 0.4.16
evalRWSI :: (Monoid w, Monad m)
         => r -> s -> Iteratee i (L.RWST r w s m) a -> Iteratee i m (a, w)
evalRWSI r s = fmap (\(x, _, w) -> (x, w)) . runRWSI r s

-- | Lifted version of (lazy) 'L.execRWST'
--
-- Since: 0.4.16
execRWSI :: (Monoid w, Monad m)
         => r -> s -> Iteratee i (L.RWST r w s m) a -> Iteratee i m (s, w)
execRWSI r s = fmap (\(_, s', w) -> (s', w)) . runRWSI r s

-- | Lifted version of (strict) 'S.runRWST'
--
-- Since: 0.4.16
runRWSI' :: (Monoid w, Monad m)
         => r -> s -> Iteratee i (S.RWST r w s m) a -> Iteratee i m (a, s, w)
runRWSI' r s0 it0 = go s0 mempty it0 where
	go s w it = Iteratee $ do
		(step, s', w') <- S.runRWST (runIteratee it) r s
		return $ case step of
			Continue k -> Continue $ go s' (w `mappend` w') . k
			Yield x cs -> Yield (x, s', w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (strict) 'S.evalRWST'
--
-- Since: 0.4.16
evalRWSI' :: (Monoid w, Monad m)
          => r -> s -> Iteratee i (S.RWST r w s m) a -> Iteratee i m (a, w)
evalRWSI' r s = fmap (\(x, _, w) -> (x, w)) . runRWSI' r s

-- | Lifted version of (strict) 'S.execRWST'
--
-- Since: 0.4.16
execRWSI' :: (Monoid w, Monad m)
          => r -> s -> Iteratee i (S.RWST r w s m) a -> Iteratee i m (s, w)
execRWSI' r s = fmap (\(_, s', w) -> (s', w)) . runRWSI' r s
