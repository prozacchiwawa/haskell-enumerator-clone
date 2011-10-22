-- |
-- Module: Data.Enumerator
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- For compatibility reasons, this module should imported qualified:
--
-- @
-- import qualified Data.Enumerator as E
-- @
module Data.Enumerator
	(
	
	-- * Types
	  Iteratee
	, Enumerator
	, Enumeratee
	
	-- ** Running iteratees
	, run
	, run_
	
	-- ** Operators
	-- | Compatibility note: Most of these will be obsoleted by
	-- @enumerator_0.5@. Please make sure your @.cabal@ files have a
	-- @<0.5@ limit on the @enumerator@ dependency.
	, (>>==)
	, (==<<)
	, ($$)
	, (>==>)
	, (<==<)
	, (=$)
	, ($=)
	
	-- ** Error handling
	, throwError
	, catchError
	
	-- * Miscellaneous
	, concatEnums
	, joinI
	, joinE
	, Data.Enumerator.sequence
	, isEOF
	, tryIO
	, liftTrans
	
	-- ** Testing and debugging
	, printChunks
	, enumList
	, enumLists
	, runLists
	, runLists_
	
	-- ** Obsolete and pointless
	, peek
	, Data.Enumerator.last
	, Data.Enumerator.length
	
	-- * Internal interfaces
	-- | This module export will be removed in @enumerator_0.5@. If you
	-- depend on internal implementation details, please import
	-- @"Data.Enumerator.Internal"@ directly.
	, module Data.Enumerator.Internal
	
	-- * Legacy compatibility
	-- | These legacy compatibility functions will be removed in
	-- @enumerator_0.5@.
	, module Data.Enumerator.Compatibility
	) where

import qualified Control.Exception as Exc
import qualified Control.Monad as CM
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.List (genericLength, genericSplitAt)

import           Data.Enumerator.Compatibility
import           Data.Enumerator.Internal

-- | Run an iteratee until it finishes, and return either the final value
-- (if it succeeded) or the error (if it failed).
--
-- > import Data.Enumerator
-- > import Data.Enumerator.List as EL
-- >
-- > main = do
-- >     result <- run (EL.iterate succ 'A' $$ EL.take 5)
-- >     case result of
-- >         Left exc -> putStrLn ("Got an exception: " ++ show exc)
-- >         Right chars -> putStrLn ("Got characters: " ++ show chars)
run :: Monad m => Iteratee a m b
    -> m (Either Exc.SomeException b)
run i = do
	mStep <- runIteratee $ enumEOF ==<< i
	case mStep of
		Error err -> return $ Left err
		Yield x _ -> return $ Right x
		Continue _ -> error "run: divergent iteratee"

-- | Like 'run', except errors are converted to exceptions and thrown.
-- Primarily useful for small scripts or other simple cases.
--
-- > import Data.Enumerator
-- > import Data.Enumerator.List as EL
-- >
-- > main = do
-- >     chars <- run_ (EL.iterate succ 'A' $$ EL.take 5)
-- >     putStrLn ("Got characters: " ++ show chars)
--
-- Since: 0.4.1
run_ :: Monad m => Iteratee a m b -> m b
run_ i = run i >>= either Exc.throw return

-- | The moral equivalent of 'Exc.throwIO', for iteratees.
throwError :: (Monad m, Exc.Exception e) => e -> Iteratee a m b
throwError exc = returnI (Error (Exc.toException exc))

-- | Runs the iteratee, and calls an exception handler if an 'Error' is
-- returned. By handling errors within the enumerator library, and requiring
-- all errors to be represented by 'Exc.SomeException', libraries with
-- varying error types can be easily composed.
--
-- WARNING: Within the error handler, it is difficult or impossible to know
-- how much input the original iteratee has consumed. Users are strongly
-- advised to wrap all uses of @catchError@ with an appropriate isolation
-- enumeratee, such as @Data.Enumerator.List.isolate@ or
-- @Data.Enumerator.Binary.isolate@, which will handle input framing even
-- in the face of unexpected errors.
--
-- Since: 0.1.1
catchError :: Monad m
           => Iteratee a m b
           -> (Exc.SomeException -> Iteratee a m b)
           -> Iteratee a m b
catchError i h = go i where
	go iter = Iteratee $ do
		step <- runIteratee iter
		case step of
			Yield _ _ -> return step
			Error err -> runIteratee (h err)
			Continue k -> return (Continue (wrap k))
	
	wrap k EOF = Iteratee $ do
		res <- run (k EOF)
		case res of
			Left err -> runIteratee (enumEOF $$ h err)
			Right b -> return (Yield b EOF)
	
	wrap k stream = Iteratee $ do
		step <- runIteratee (k stream)
		case step of
			Yield _ _ -> return step
			Error err -> do
				step' <- runIteratee (h err)
				case step' of
					Continue k' -> runIteratee (k' stream)
					_ -> return step'
			Continue k' -> return (Continue (wrap k'))

-- | Print chunks as they're received from the enumerator, optionally
-- printing empty chunks.
printChunks :: (MonadIO m, Show a)
            => Bool -- ^ Print empty chunks
            -> Iteratee a m ()
printChunks printEmpty = continue loop where
	loop (Chunks xs) = do
		let hide = null xs && not printEmpty
		CM.unless hide (liftIO (print xs))
		continue loop
	
	loop EOF = do
		liftIO (putStrLn "EOF")
		yield () EOF

-- | @'enumList' n xs@ enumerates /xs/ as a stream, passing /n/ inputs per
-- chunk. This is primarily useful for testing, debugging, and REPL
-- exploration.
--
-- Compatibility note: In @enumerator_0.5@, 'enumList' will be changed to the
-- type:
--
-- > enumList :: Monad m => [a] -> Enumerator a m b
enumList :: Monad m => Integer -> [a] -> Enumerator a m b
enumList n = loop where
	loop xs (Continue k) | not (null xs) = let
		(s1, s2) = genericSplitAt n xs
		in k (Chunks s1) >>== loop s2
	loop _ step = returnI step

-- | @'enumLists' xs@ enumerates /xs/ as a stream, where each element is a
-- separate chunk. This is primarily useful for testing and debugging.
--
-- Since: 0.4.15
enumLists :: Monad m => [[a]] -> Enumerator a m b
enumLists (xs:xss) (Continue k) = k (Chunks xs) >>== enumLists xss
enumLists _ step = returnI step

-- | Run an iteratee with the given input, and return either the final value
-- (if it succeeded) or the error (if it failed).
--
-- Since: 0.4.15
runLists :: [[a]] -> Iteratee a Identity b -> Either Exc.SomeException b
runLists lists iter = runIdentity (run (enumLists lists $$ iter))

-- | Like 'runLists', except errors are converted to exceptions and thrown.
--
-- Since: 0.4.15
runLists_ :: [[a]] -> Iteratee a Identity b -> b
runLists_ lists iter = runIdentity (run_ (enumLists lists $$ iter))

-- | Compose a list of 'Enumerator's using @('>==>').@
concatEnums :: Monad m => [Enumerator a m b]
            -> Enumerator a m b
concatEnums = Prelude.foldl (>==>) returnI

-- | &#x201c;Wraps&#x201d; an iteratee /inner/ in an enumeratee /wrapper/.
-- The resulting iteratee will consume /wrapper/&#x2019;s input type and
-- yield /inner/&#x2019;s output type.
--
-- See the documentation for ('=$').
--
-- @joinI (enum $$ iter) = enum =$ iter@
joinI :: Monad m => Iteratee a m (Step a' m b)
      -> Iteratee a m b
joinI outer = outer >>= check where
	check (Continue k) = k EOF >>== \s -> case s of
		Continue _ -> error "joinI: divergent iteratee"
		_ -> check s
	check (Yield x _) = return x
	check (Error e) = throwError e

infixr 0 =$

-- | &#x201c;Wraps&#x201d; an iteratee /inner/ in an enumeratee /wrapper/.
-- The resulting iteratee will consume /wrapper/&#x2019;s input type and
-- yield /inner/&#x2019;s output type.
--
-- Note: if the inner iteratee yields leftover input when it finishes,
-- that extra will be discarded.
--
-- As an example, consider an iteratee that converts a stream of UTF8-encoded
-- bytes into a single @Text@:
--
-- > consumeUTF8 :: Monad m => Iteratee ByteString m Text
--
-- It could be written with either 'joinI' or @(=$)@:
--
-- > import Data.Enumerator.Text as ET
-- >
-- > consumeUTF8 = joinI (decode utf8 $$ ET.consume)
-- > consumeUTF8 = decode utf8 =$ ET.consume
--
-- Since: 0.4.9
(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
enum =$ iter = joinI (enum $$ iter)

-- | &#x201c;Wraps&#x201d; an enumerator /inner/ in an enumeratee /wrapper/.
-- The resulting enumerator will generate /wrapper/&#x2019;s output type.
--
-- See the documentation for ('$=').
--
-- @joinE enum enee = enum $= enee@
--
-- Since: 0.4.5
joinE :: Monad m
      => Enumerator ao m (Step ai m b)
      -> Enumeratee ao ai m b
      -> Enumerator ai m b
joinE enum enee s = Iteratee $ do
	step <- runIteratee (enumEOF $$ enum $$ enee s)
	case step of
		Error err -> return (Error err)
		Yield x _ -> return x
		Continue _ -> error "joinE: divergent iteratee"

infixl 1 $=

-- | &#x201c;Wraps&#x201d; an enumerator /inner/ in an enumeratee /wrapper/.
-- The resulting enumerator will generate /wrapper/&#x2019;s output type.
--
-- As an example, consider an enumerator that yields line character counts
-- for a text file (e.g. for source code readability checking):
--
-- > enumFileCounts :: FilePath -> Enumerator Int IO b
--
-- It could be written with either 'joinE' or '($=)':
--
-- > import Data.Text as T
-- > import Data.Enumerator.List as EL
-- > import Data.Enumerator.Text as ET
-- >
-- > enumFileCounts path = joinE (enumFile path) (EL.map T.length)
-- > enumFileCounts path = enumFile path $= EL.map T.length
--
-- Since: 0.4.9
($=) :: Monad m
     => Enumerator ao m (Step ai m b)
     -> Enumeratee ao ai m b
     -> Enumerator ai m b
($=) = joinE

-- | Feeds outer input elements into the provided iteratee until it yields
-- an inner input, passes that to the inner iteratee, and then loops.
sequence :: Monad m => Iteratee ao m ai
         -> Enumeratee ao ai m b
sequence i = loop where
	loop = checkDone check
	check k = isEOF >>= \f -> if f
		then yield (Continue k) EOF
		else step k
	step k = i >>= \v -> k (Chunks [v]) >>== loop

-- | Check whether a stream has reached EOF. Note that if the stream is not
-- at EOF, @isEOF@ may cause data to be read from the enumerator.
isEOF :: Monad m => Iteratee a m Bool
isEOF = continue $ \s -> case s of
	EOF -> yield True s
	_ -> yield False s

-- | Try to run an IO computation. If it throws an exception, the exception
-- is caught and passed to 'throwError'.
--
-- Since: 0.4.9
tryIO :: MonadIO m => IO b -> Iteratee a m b
tryIO io = Iteratee $ do
	tried <- liftIO (Exc.try io)
	return $ case tried of
		Right b -> Yield b (Chunks [])
		Left err -> Error err

-- | Lift an 'Iteratee' onto a monad transformer, re-wrapping its
-- inner monadic values.
--
-- Since: 0.1.1
liftTrans :: (Monad m, MonadTrans t, Monad (t m)) =>
             Iteratee a m b -> Iteratee a (t m) b
liftTrans iter = Iteratee $ do
	step <- lift (runIteratee iter)
	return $ case step of
		Yield x cs -> Yield x cs
		Error err -> Error err
		Continue k -> Continue (liftTrans . k)

-- | Peek at the next element in the stream, or 'Nothing' if the stream
-- has ended.
peek :: Monad m => Iteratee a m (Maybe a)
peek = continue loop where
	loop (Chunks []) = continue loop
	loop chunk@(Chunks (x:_)) = yield (Just x) chunk
	loop EOF = yield Nothing EOF

-- | Get the last element in the stream, or 'Nothing' if the stream
-- has ended.
--
-- Consumes the entire stream.
last :: Monad m => Iteratee a m (Maybe a)
last = continue (loop Nothing) where
	loop ret (Chunks xs) = continue . loop $ case xs of
		[] -> ret
		_ -> Just (Prelude.last xs)
	loop ret EOF = yield ret EOF

-- | Get how many elements remained in the stream.
--
-- Consumes the entire stream.
length :: Monad m => Iteratee a m Integer
length = continue (loop 0) where
	len = genericLength
	loop n (Chunks xs) = continue (loop (n + len xs))
	loop n EOF = yield n EOF
