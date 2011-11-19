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
	( runIdentity
	, runMaybe
	, runError
	, runReader
	, runState
	, evalState
	, runState'
	, evalState'
	, runWriter
	, execWriter
	, runWriter'
	, execWriter'
	, runRWS
	, evalRWS
	, execRWS
	, runRWS'
	, evalRWS'
	, execRWS'
	) where

import           Data.Monoid (Monoid(..))
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Reader hiding (runReader)
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
runIdentity :: Monad m => Iteratee i (IdentityT m) a -> Iteratee i m a
runIdentity it = Iteratee $ do
	step <- runIdentityT $ runIteratee it
	return $ case step of
		Continue k -> Continue $ runIdentity . k
		Yield x cs -> Yield x cs
		Error e    -> Error e

-- | Lifted version of 'runMaybeT'
--
-- Since: 0.4.16
runMaybe :: Monad m => Iteratee i (MaybeT m) a -> Iteratee i m (Maybe a)
runMaybe it = Iteratee $ do
	mStep <- runMaybeT $ runIteratee it
	return $ case mStep of
		Nothing   -> Yield Nothing $ Chunks []
		Just step -> case step of
			Continue k -> Continue $ runMaybe . k
			Yield x cs -> Yield (Just x) cs
			Error e    -> Error e

-- | Lifted version of 'runErrorT'
--
-- Since: 0.4.16
runError :: (Error e, Monad m)
          => Iteratee i (ErrorT e m) a -> Iteratee i m (Either e a)
runError it = Iteratee $ do
	mStep <- runErrorT $ runIteratee it
	return $ case mStep of
		Left e     -> Yield (Left e) $ Chunks []
		Right step -> case step of
			Continue k -> Continue $ runError . k
			Yield x cs -> Yield (Right x) cs
			Error e    -> Error e

-- | Lifted version of 'runReaderT'
--
-- Since: 0.4.16
runReader :: Monad m => r -> Iteratee i (ReaderT r m) a -> Iteratee i m a
runReader r it = Iteratee $ do
	step <- runReaderT (runIteratee it) r
	return $ case step of
		Continue k -> Continue $ runReader r . k
		Yield x cs -> Yield x cs
		Error e    -> Error e

-- | Lifted version of (lazy) 'L.runStateT'
--
-- Since: 0.4.16
runState :: Monad m => s -> Iteratee i (L.StateT s m) a -> Iteratee i m (a, s)
runState s it = Iteratee $ do
	~(step, s') <- L.runStateT (runIteratee it) s
	return $ case step of
		Continue k -> Continue $ runState s' . k
		Yield x cs -> Yield (x, s') cs
		Error e    -> Error e

-- | Lifted version of (lazy) 'L.evalStateT'
--
-- Since: 0.4.16
evalState :: Monad m => s -> Iteratee i (L.StateT s m) a -> Iteratee i m a
evalState s = fmap fst . runState s

-- | Lifted version of (strict) 'S.runStateT'
--
-- Since: 0.4.16
runState' :: Monad m => s -> Iteratee i (S.StateT s m) a -> Iteratee i m (a, s)
runState' s it = Iteratee $ do
	(step, s') <- S.runStateT (runIteratee it) s
	return $ case step of
		Continue k -> Continue $ runState' s' . k
		Yield x cs -> Yield (x, s') cs
		Error e    -> Error e

-- | Lifted version of (strict) 'S.evalStateT'
--
-- Since: 0.4.16
evalState' :: Monad m => s -> Iteratee i (S.StateT s m) a -> Iteratee i m a
evalState' s = fmap fst . runState' s

-- | Lifted version of (lazy) 'L.runWriterT'
--
-- Since: 0.4.16
runWriter :: (Monoid w, Monad m)
           => Iteratee i (L.WriterT w m) a -> Iteratee i m (a, w)
runWriter it0 = go mempty it0 where
	go w it = Iteratee $ do
		~(step, w') <- L.runWriterT $ runIteratee it
		return $ case step of
			Continue k -> Continue $ go (w `mappend` w') . k
			Yield x cs -> Yield (x, w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (lazy) 'L.execWriterT'
--
-- Since: 0.4.16
execWriter :: (Monoid w, Monad m)
            => Iteratee i (L.WriterT w m) a -> Iteratee i m w
execWriter = fmap snd . runWriter

-- | Lifted version of (strict) 'S.runWriterT'
--
-- Since: 0.4.16
runWriter' :: (Monoid w, Monad m)
            => Iteratee i (S.WriterT w m) a -> Iteratee i m (a, w)
runWriter' it0 = go mempty it0 where
	go w it = Iteratee $ do
		(step, w') <- S.runWriterT $ runIteratee it
		return $ case step of
			Continue k -> Continue $ go (w `mappend` w') . k
			Yield x cs -> Yield (x, w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (strict) 'L.execWriterT'
--
-- Since: 0.4.16
execWriter' :: (Monoid w, Monad m)
             => Iteratee i (S.WriterT w m) a -> Iteratee i m w
execWriter' = fmap snd . runWriter'

-- | Lifted version of (lazy) 'L.runRWST'
--
-- Since: 0.4.16
runRWS :: (Monoid w, Monad m)
        => r -> s -> Iteratee i (L.RWST r w s m) a -> Iteratee i m (a, s, w)
runRWS r s0 it0 = go s0 mempty it0 where
	go s w it = Iteratee $ do
		~(step, s', w') <- L.runRWST (runIteratee it) r s
		return $ case step of
			Continue k -> Continue $ go s' (w `mappend` w') . k
			Yield x cs -> Yield (x, s', w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (lazy) 'L.evalRWST'
--
-- Since: 0.4.16
evalRWS :: (Monoid w, Monad m)
         => r -> s -> Iteratee i (L.RWST r w s m) a -> Iteratee i m (a, w)
evalRWS r s = fmap (\(x, _, w) -> (x, w)) . runRWS r s

-- | Lifted version of (lazy) 'L.execRWST'
--
-- Since: 0.4.16
execRWS :: (Monoid w, Monad m)
         => r -> s -> Iteratee i (L.RWST r w s m) a -> Iteratee i m (s, w)
execRWS r s = fmap (\(_, s', w) -> (s', w)) . runRWS r s

-- | Lifted version of (strict) 'S.runRWST'
--
-- Since: 0.4.16
runRWS' :: (Monoid w, Monad m)
         => r -> s -> Iteratee i (S.RWST r w s m) a -> Iteratee i m (a, s, w)
runRWS' r s0 it0 = go s0 mempty it0 where
	go s w it = Iteratee $ do
		(step, s', w') <- S.runRWST (runIteratee it) r s
		return $ case step of
			Continue k -> Continue $ go s' (w `mappend` w') . k
			Yield x cs -> Yield (x, s', w `mappend` w') cs
			Error e    -> Error e

-- | Lifted version of (strict) 'S.evalRWST'
--
-- Since: 0.4.16
evalRWS' :: (Monoid w, Monad m)
          => r -> s -> Iteratee i (S.RWST r w s m) a -> Iteratee i m (a, w)
evalRWS' r s = fmap (\(x, _, w) -> (x, w)) . runRWS' r s

-- | Lifted version of (strict) 'S.execRWST'
--
-- Since: 0.4.16
execRWS' :: (Monoid w, Monad m)
          => r -> s -> Iteratee i (S.RWST r w s m) a -> Iteratee i m (s, w)
execRWS' r s = fmap (\(_, s', w) -> (s', w)) . runRWS' r s
