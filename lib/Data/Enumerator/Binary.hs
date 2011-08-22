-- |
-- Module: Data.Enumerator.Binary
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Byte-oriented alternatives to "Data.Enumerator.List". Note that the
-- enumeratees in this module must unpack their inputs to work properly. If
-- you do not need to handle leftover input on a byte-by-byte basis, the
-- chunk-oriented versions will be much faster.
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Data.Enumerator.Binary as EB
-- @
--
-- Since: 0.4.5
module Data.Enumerator.Binary
	(
	
	-- * IO
	  enumHandle
	, enumHandleRange
	, enumFile
	, enumFileRange
	, iterHandle
	
	-- * List analogues
	
	-- ** Folds
	, fold
	, foldM
	
	-- ** Maps
	, Data.Enumerator.Binary.map
	, Data.Enumerator.Binary.mapM
	, Data.Enumerator.Binary.mapM_
	, Data.Enumerator.Binary.concatMap
	, concatMapM
	
	-- ** Accumulating maps
	, mapAccum
	, mapAccumM
	, concatMapAccum
	, concatMapAccumM
	
	-- ** Infinite streams
	, Data.Enumerator.Binary.iterate
	, iterateM
	, Data.Enumerator.Binary.repeat
	, repeatM
	
	-- ** Bounded streams
	, Data.Enumerator.Binary.replicate
	, replicateM
	, generateM
	, unfold
	, unfoldM
	
	-- ** Filters
	, Data.Enumerator.Binary.filter
	, filterM
	
	-- ** Consumers
	, Data.Enumerator.Binary.take
	, takeWhile
	, consume
	
	-- ** Unsorted
	, Data.Enumerator.Binary.head
	, Data.Enumerator.Binary.drop
	, Data.Enumerator.Binary.dropWhile
	, require
	, isolate
	, splitWhen
	
	) where

import           Prelude hiding (head, drop, takeWhile, mapM_)
import qualified Control.Exception as Exc
import qualified Control.Monad as CM
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word8)

import qualified System.IO as IO
import           System.IO.Error (isEOFError)

import           Data.Enumerator hiding ( head, drop, iterateM, repeatM, replicateM
                                        , generateM, filterM, consume, foldM
                                        , concatMapM)
import qualified Data.Enumerator.List as EL

-- | Consume the entire input stream with a strict left fold, one byte
-- at a time.
--
-- Since: 0.4.8
fold :: Monad m => (b -> Word8 -> b) -> b
     -> Iteratee B.ByteString m b
fold step = EL.fold (B.foldl' step)

-- | Consume the entire input stream with a strict monadic left fold, one
-- byte at a time.
--
-- Since: 0.4.8
foldM :: Monad m => (b -> Word8 -> m b) -> b
      -> Iteratee B.ByteString m b
foldM step = EL.foldM (\b bytes -> CM.foldM step b (B.unpack bytes))

-- | Enumerates a stream of bytes by repeatedly applying a function to
-- some state.
--
-- Similar to 'Data.Enumerator.Binary.iterate'.
--
-- Since: 0.4.8
unfold :: Monad m => (s -> Maybe (Word8, s)) -> s -> Enumerator B.ByteString m b
unfold f = checkContinue1 $ \loop s k -> case f s of
	Nothing -> continue k
	Just (b, s') -> k (Chunks [B.singleton b]) >>== loop s'

-- | Enumerates a stream of bytes by repeatedly applying a computation to
-- some state.
--
-- Similar to 'iterateM'.
--
-- Since: 0.4.8
unfoldM :: Monad m => (s -> m (Maybe (Word8, s))) -> s -> Enumerator B.ByteString m b
unfoldM f = checkContinue1 $ \loop s k -> do
	fs <- lift (f s)
	case fs of
		Nothing -> continue k
		Just (b, s') -> k (Chunks [B.singleton b]) >>== loop s'

-- | @'Data.Enumerator.Binary.map' f@ applies /f/ to each input byte and
-- feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
map :: Monad m => (Word8 -> Word8) -> Enumeratee B.ByteString B.ByteString m b
map f = Data.Enumerator.Binary.concatMap (\x -> B.singleton (f x))

-- | @'Data.Enumerator.Binary.mapM' f@ applies /f/ to each input byte and
-- feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
mapM :: Monad m => (Word8 -> m Word8) -> Enumeratee B.ByteString B.ByteString m b
mapM f = Data.Enumerator.Binary.concatMapM (\x -> liftM B.singleton (f x))

-- | @'Data.Enumerator.Binary.mapM_' f@ applies /f/ to each input byte, and
-- discards the results.
--
-- Since: 0.4.11
mapM_ :: Monad m => (Word8 -> m ()) -> Iteratee B.ByteString m ()
mapM_ f = foldM (\_ x -> f x >> return ()) ()

-- | @'Data.Enumerator.Binary.concatMap' f@ applies /f/ to each input byte
-- and feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
concatMap :: Monad m => (Word8 -> B.ByteString) -> Enumeratee B.ByteString B.ByteString m b
concatMap f = Data.Enumerator.Binary.concatMapM (return . f)

-- | @'concatMapM' f@ applies /f/ to each input byte and feeds the
-- resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
concatMapM :: Monad m => (Word8 -> m B.ByteString) -> Enumeratee B.ByteString B.ByteString m b
concatMapM f = checkDone (continue . step) where
	step k EOF = yield (Continue k) EOF
	step k (Chunks xs) = loop k (BL.unpack (BL.fromChunks xs))
	
	loop k [] = continue (step k)
	loop k (x:xs) = do
		fx <- lift (f x)
		k (Chunks [fx]) >>==
			checkDoneEx (Chunks [B.pack xs]) (\k' -> loop k' xs)

-- | Similar to 'Data.Enumerator.Binary.concatMap', but with a stateful step
-- function.
--
-- Since: 0.4.11
concatMapAccum :: Monad m => (s -> Word8 -> (s, B.ByteString)) -> s -> Enumeratee B.ByteString B.ByteString m b
concatMapAccum f s0 = checkDone (continue . step s0) where
	step _ k EOF = yield (Continue k) EOF
	step s k (Chunks xs) = loop s k xs
	
	loop s k [] = continue (step s k)
	loop s k (x:xs) = case B.uncons x of
		Nothing -> loop s k xs
		Just (b, x') -> case f s b of
			(s', ai) -> k (Chunks [ai]) >>==
				checkDoneEx (Chunks (x':xs)) (\k' -> loop s' k' (x':xs))

-- | Similar to 'concatMapM', but with a stateful step function.
--
-- Since: 0.4.11
concatMapAccumM :: Monad m => (s -> Word8 -> m (s, B.ByteString)) -> s -> Enumeratee B.ByteString B.ByteString m b
concatMapAccumM f s0 = checkDone (continue . step s0) where
	step _ k EOF = yield (Continue k) EOF
	step s k (Chunks xs) = loop s k xs
	
	loop s k [] = continue (step s k)
	loop s k (x:xs) = case B.uncons x of
		Nothing -> loop s k xs
		Just (b, x') -> do
			(s', ai) <- lift (f s b)
			k (Chunks [ai]) >>==
				checkDoneEx (Chunks (x':xs)) (\k' -> loop s' k' (x':xs))

-- | Similar to 'Data.Enumerator.Binary.map', but with a stateful step
-- function.
--
-- Since: 0.4.9
mapAccum :: Monad m => (s -> Word8 -> (s, Word8)) -> s -> Enumeratee B.ByteString B.ByteString m b
mapAccum f = concatMapAccum (\s w -> case f s w of (s', w') -> (s', B.singleton w'))

-- | Similar to 'Data.Enumerator.Binary.mapM', but with a stateful step
-- function.
--
-- Since: 0.4.9
mapAccumM :: Monad m => (s -> Word8 -> m (s, Word8)) -> s -> Enumeratee B.ByteString B.ByteString m b
mapAccumM f = concatMapAccumM (\s w -> do
	(s', w') <- f s w
	return (s', B.singleton w'))

-- | @'Data.Enumerator.Binary.iterate' f x@ enumerates an infinite stream of
-- repeated applications of /f/ to /x/.
--
-- Analogous to 'Prelude.iterate'.
--
-- Since: 0.4.8
iterate :: Monad m => (Word8 -> Word8) -> Word8 -> Enumerator B.ByteString m b
iterate f = checkContinue1 $ \loop s k -> k (Chunks [B.singleton s]) >>== loop (f s)

-- | Similar to 'Data.Enumerator.Binary.iterate', except the iteration
-- function is monadic.
--
-- Since: 0.4.8
iterateM :: Monad m => (Word8 -> m Word8) -> Word8 -> Enumerator B.ByteString m b
iterateM f base = worker (return base) where
	worker = checkContinue1 $ \loop m_byte k -> do
		byte <- lift m_byte
		k (Chunks [B.singleton byte]) >>== loop (f byte)

-- | Enumerates an infinite stream of a single byte.
--
-- Analogous to 'Prelude.repeat'.
--
-- Since: 0.4.8
repeat :: Monad m => Word8 -> Enumerator B.ByteString m b
repeat byte = EL.repeat (B.singleton byte)

-- | Enumerates an infinite stream of byte. Each byte is computed by the
-- underlying monad.
--
-- Since: 0.4.8
repeatM :: Monad m => m Word8 -> Enumerator B.ByteString m b
repeatM next = EL.repeatM (liftM B.singleton next)

-- | @'Data.Enumerator.Binary.replicate' n x@ enumerates a stream containing
-- /n/ copies of /x/.
--
-- Since: 0.4.8
replicate :: Monad m => Integer -> Word8 -> Enumerator B.ByteString m b
replicate n byte = EL.replicate n (B.singleton byte)

-- | @'replicateM' n m_x@ enumerates a stream of /n/ bytes, with each byte
-- computed by /m_x/.
--
-- Since: 0.4.8
replicateM :: Monad m => Integer -> m Word8 -> Enumerator B.ByteString m b
replicateM n next = EL.replicateM n (liftM B.singleton next)

-- | Like 'repeatM', except the computation may terminate the stream by
-- returning 'Nothing'.
--
-- Since: 0.4.8
generateM :: Monad m => m (Maybe Word8) -> Enumerator B.ByteString m b
generateM next = EL.generateM (liftM (liftM B.singleton) next)

-- | Applies a predicate to the stream. The inner iteratee only receives
-- characters for which the predicate is @True@.
--
-- Since: 0.4.8
filter :: Monad m => (Word8 -> Bool) -> Enumeratee B.ByteString B.ByteString m b
filter p = Data.Enumerator.Binary.concatMap (\x -> B.pack [x | p x])

-- | Applies a monadic predicate to the stream. The inner iteratee only
-- receives bytes for which the predicate returns @True@.
--
-- Since: 0.4.8
filterM :: Monad m => (Word8 -> m Bool) -> Enumeratee B.ByteString B.ByteString m b
filterM p = Data.Enumerator.Binary.concatMapM (\x -> liftM B.pack (CM.filterM p [x]))

-- | @'Data.Enumerator.Binary.take' n@ extracts the next /n/ bytes from the
-- stream, as a lazy
-- ByteString.
--
-- Since: 0.4.5
take :: Monad m => Integer -> Iteratee B.ByteString m BL.ByteString
take n | n <= 0 = return BL.empty
take n = continue (loop id n) where
	loop acc n' (Chunks xs) = iter where
		lazy = BL.fromChunks xs
		len = toInteger (BL.length lazy)
		
		iter = if len < n'
			then continue (loop (acc . (BL.append lazy)) (n' - len))
			else let
				(xs', extra) = BL.splitAt (fromInteger n') lazy
				in yield (acc xs') (toChunks extra)
	loop acc _ EOF = yield (acc BL.empty) EOF

-- | @'takeWhile' p@ extracts input from the stream until the first byte which
-- does not match the predicate.
--
-- Since: 0.4.5
takeWhile :: Monad m => (Word8 -> Bool) -> Iteratee B.ByteString m BL.ByteString
takeWhile p = continue (loop id) where
	loop acc (Chunks []) = continue (loop acc)
	loop acc (Chunks xs) = iter where
		lazy = BL.fromChunks xs
		(xs', extra) = BL.span p lazy
		iter = if BL.null extra
			then continue (loop (acc . (BL.append lazy)))
			else yield (acc xs') (toChunks extra)
	loop acc EOF = yield (acc BL.empty) EOF

-- | @'consume' = 'takeWhile' (const True)@
--
-- Since: 0.4.5
consume :: Monad m => Iteratee B.ByteString m BL.ByteString
consume = continue (loop id) where
	loop acc (Chunks []) = continue (loop acc)
	loop acc (Chunks xs) = iter where
		lazy = BL.fromChunks xs
		iter = continue (loop (acc . (BL.append lazy)))
	loop acc EOF = yield (acc BL.empty) EOF

-- | Get the next byte from the stream, or 'Nothing' if the stream has
-- ended.
--
-- Since: 0.4.5
head :: Monad m => Iteratee B.ByteString m (Maybe Word8)
head = continue loop where
	loop (Chunks xs) = case BL.uncons (BL.fromChunks xs) of
		Just (char, extra) -> yield (Just char) (toChunks extra)
		Nothing -> head
	loop EOF = yield Nothing EOF

-- | @'drop' n@ ignores /n/ bytes of input from the stream.
--
-- Since: 0.4.5
drop :: Monad m => Integer -> Iteratee B.ByteString m ()
drop n | n <= 0 = return ()
drop n = continue (loop n) where
	loop n' (Chunks xs) = iter where
		lazy = BL.fromChunks xs
		len = toInteger (BL.length lazy)
		iter = if len < n'
			then drop (n' - len)
			else yield () (toChunks (BL.drop (fromInteger n') lazy))
	loop _ EOF = yield () EOF

-- | @'Data.Enumerator.Binary.dropWhile' p@ ignores input from the stream
-- until the first byte which does not match the predicate.
--
-- Since: 0.4.5
dropWhile :: Monad m => (Word8 -> Bool) -> Iteratee B.ByteString m ()
dropWhile p = continue loop where
	loop (Chunks xs) = iter where
		lazy = BL.dropWhile p (BL.fromChunks xs)
		iter = if BL.null lazy
			then continue loop
			else yield () (toChunks lazy)
	loop EOF = yield () EOF

-- | @'require' n@ buffers input until at least /n/ bytes are available, or
-- throws an error if the stream ends early.
--
-- Since: 0.4.5
require :: Monad m => Integer -> Iteratee B.ByteString m ()
require n | n <= 0 = return ()
require n = continue (loop id n) where
	loop acc n' (Chunks xs) = iter where
		lazy = BL.fromChunks xs
		len = toInteger (BL.length lazy)
		iter = if len < n'
			then continue (loop (acc . (BL.append lazy)) (n' - len))
			else yield () (toChunks (acc lazy))
	loop _ _ EOF = throwError (Exc.ErrorCall "require: Unexpected EOF")

-- | @'isolate' n@ reads at most /n/ bytes from the stream, and passes them
-- to its iteratee. If the iteratee finishes early, bytes continue to be
-- consumed from the outer stream until /n/ have been consumed.
--
-- Since: 0.4.5
isolate :: Monad m => Integer -> Enumeratee B.ByteString B.ByteString m b
isolate n step | n <= 0 = return step
isolate n (Continue k) = continue loop where
	loop (Chunks []) = continue loop
	loop (Chunks xs) = iter where
		lazy = BL.fromChunks xs
		len = toInteger (BL.length lazy)
		
		iter = if len <= n
			then k (Chunks xs) >>== isolate (n - len)
			else let
				(s1, s2) = BL.splitAt (fromInteger n) lazy
				in k (toChunks s1) >>== (\step -> yield step (toChunks s2))
	loop EOF = k EOF >>== (\step -> yield step EOF)
isolate n step = drop n >> return step

-- | Split on bytes satisfying a given predicate.
--
-- Since: 0.4.8
splitWhen :: Monad m => (Word8 -> Bool) -> Enumeratee B.ByteString B.ByteString m b
splitWhen p = loop where
	loop = checkDone step
	step k = isEOF >>= \eof -> if eof
		then yield (Continue k) EOF
		else do
			lazy <- takeWhile (not . p)
			let bytes = B.concat (BL.toChunks lazy)
			eof <- isEOF
			drop 1
			if BL.null lazy && eof
				then yield (Continue k) EOF
				else k (Chunks [bytes]) >>== loop

-- | Read bytes (in chunks of the given buffer size) from the handle, and
-- stream them to an 'Iteratee'. If an exception occurs during file IO,
-- enumeration will stop and 'Error' will be returned. Exceptions from the
-- iteratee are not caught.
--
-- This enumerator blocks until at least one byte is available from the
-- handle, and might read less than the maximum buffer size in some
-- cases.
--
-- The handle should be opened with no encoding, and in 'IO.ReadMode' or
-- 'IO.ReadWriteMode'.
--
-- Since: 0.4.5
enumHandle :: MonadIO m
           => Integer -- ^ Buffer size
           -> IO.Handle
           -> Enumerator B.ByteString m b
enumHandle bufferSize h = checkContinue0 $ \loop k -> do
	let intSize = fromInteger bufferSize
	
	bytes <- tryIO (getBytes h intSize)
	if B.null bytes
		then continue k
		else k (Chunks [bytes]) >>== loop

-- | Read bytes (in chunks of the given buffer size) from the handle, and
-- stream them to an 'Iteratee'. If an exception occurs during file IO,
-- enumeration will stop and 'Error' will be returned. Exceptions from the
-- iteratee are not caught.
--
-- This enumerator blocks until at least one byte is available from the
-- handle, and might read less than the maximum buffer size in some
-- cases.
--
-- The handle should be opened with no encoding, and in 'IO.ReadMode' or
-- 'IO.ReadWriteMode'.
--
-- If an offset is specified, the handle will be seeked to that offset
-- before reading. If the handle cannot be seeked, an error will be
-- thrown.
--
-- If a maximum count is specified, the number of bytes read will not
-- exceed that count.
--
-- Since: 0.4.8
enumHandleRange :: MonadIO m
                => Integer -- ^ Buffer size
                -> Maybe Integer -- ^ Offset
                -> Maybe Integer -- ^ Maximum count
                -> IO.Handle
                -> Enumerator B.ByteString m b
enumHandleRange bufferSize offset count h s = seek >> enum where
	seek = case offset of
		Nothing -> return ()
		Just off -> tryIO (IO.hSeek h IO.AbsoluteSeek off)
	
	enum = case count of
		Just n -> enumRange n s
		Nothing -> enumHandle bufferSize h s
	
	enumRange = checkContinue1 $ \loop n k -> let
		rem = fromInteger (min bufferSize n)
		keepGoing = do
			bytes <- tryIO (getBytes h rem)
			if B.null bytes
				then continue k
				else feed bytes
		feed bs = k (Chunks [bs]) >>== loop (n - (toInteger (B.length bs)))
		in if rem <= 0
			then continue k
			else keepGoing

getBytes :: IO.Handle -> Int -> IO B.ByteString
getBytes h n = do
	hasInput <- Exc.catch
		(IO.hWaitForInput h (-1))
		(\err -> if isEOFError err
			then return False
			else Exc.throwIO err)
	if hasInput
		then B.hGetNonBlocking h n
		else return B.empty

-- | Opens a file path in binary mode, and passes the handle to
-- 'enumHandle'. The file will be closed when enumeration finishes.
--
-- Since: 0.4.5
enumFile :: FilePath -> Enumerator B.ByteString IO b
enumFile path = enumFileRange path Nothing Nothing

-- | Opens a file path in binary mode, and passes the handle to
-- 'enumHandleRange'. The file will be closed when enumeration finishes.
--
-- Since: 0.4.8
enumFileRange :: FilePath
              -> Maybe Integer -- ^ Offset
              -> Maybe Integer -- ^ Maximum count
              -> Enumerator B.ByteString IO b
enumFileRange path offset count step = do
	h <- tryIO (IO.openBinaryFile path IO.ReadMode)
	let iter = enumHandleRange 4096 offset count h step
	Iteratee (Exc.finally (runIteratee iter) (IO.hClose h))

-- | Read bytes from a stream and write them to a handle. If an exception
-- occurs during file IO, enumeration will stop and 'Error' will be
-- returned.
--
-- The handle should be opened with no encoding, and in 'IO.WriteMode' or
-- 'IO.ReadWriteMode'.
--
-- Since: 0.4.5
iterHandle :: MonadIO m => IO.Handle
           -> Iteratee B.ByteString m ()
iterHandle h = continue step where
	step EOF = yield () EOF
	step (Chunks []) = continue step
	step (Chunks bytes) = do
		tryIO (CM.mapM_ (B.hPut h) bytes)
		continue step


toChunks :: BL.ByteString -> Stream B.ByteString
toChunks = Chunks . BL.toChunks
