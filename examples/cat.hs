-----------------------------------------------------------------------------
-- |
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-----------------------------------------------------------------------------
module Main (main) where
import Prelude as Prelude
import Control.Exception as E
import Control.Monad.IO.Class (liftIO)
import Data.Enumerator
import qualified Data.ByteString as B
import qualified Foreign as F
import System.IO
import System.IO.Error (isEOFError)
import System.Environment (getArgs)

-- The following definitions of 'enumHandle', 'enumFile', and 'iterHandle' are
-- copied from "Data.Enumerator.Binary", with additional comments so they're
-- easier to understand.

enumHandle :: Integer -- ^ Buffer size
           -> Handle
           -> Enumerator B.ByteString IO b
enumHandle bufferSize h = loop where
	intSize = fromInteger bufferSize
	
	-- If more input is required before the enumerator's iteratee can
	-- yield a result, feed it from the handle.
	loop (Continue k) = do
		-- While not strictly necessary to proper operation, catching
		-- exceptions here allows more unified exception handling when
		-- the enumerator/iteratee is run.
		eitherBytes <- liftIO $ E.try $ do
			
			-- The enumerator must function normally when the
			-- handle is something like a slow file, or network
			-- socket; if there's not enough data to fill the
			-- buffer yet, a partial read is returned.
			hasInput <- E.catch
				(hWaitForInput h (1))
				(\err -> if isEOFError err
					then return False
					else E.throwIO err)
			
			-- An EOF is represented by the empty bytestring
			if hasInput
				then B.hGetNonBlocking h intSize
				else return B.empty
			
		case eitherBytes of
			-- Interacting with the socket threw an IO error of
			-- some sort
			Left err -> throwError (err :: E.SomeException)
			
			-- The socket has reached EOF; pass control to the
			-- next enumerator
			Right bytes | B.null bytes -> continue k
			
			-- Bytes were read successfully; feed them to the
			-- iteratee and continue looping
			Right bytes -> k (Chunks [bytes]) >>== loop
	
	-- If a different step is received ('Error' or 'Yield'), just pass
	-- it through.
	loop step = returnI step

enumFile :: FilePath -> Enumerator B.ByteString IO b
enumFile path s = do
	-- Opening the file can be performed either inside or outside of the
	-- Iteratee. Inside allows exceptions to be caught and propagated
	-- through the 'Error' step constructor.
	eitherH <- liftIO (E.try (openBinaryFile path ReadMode))
	case eitherH of
		Left err -> throwError (err :: E.SomeException)
		Right h -> Iteratee $ finally
			(runIteratee (enumHandle 4096 h s))
			(hClose h)

-- 'iterHandle' is the opposite of 'enumHandle', in that it *writes to* a
-- handle instead of reading from it. An enumerator is a source, an iteratee
-- is a sink.
iterHandle :: Handle -> Iteratee B.ByteString IO ()

-- Most iteratees start in the 'Continue' state, as they need some
-- input before they can produce any value.
iterHandle h = continue step where
	
	-- This iteratee produces no value; its only purpose is its
	-- side-effects. When 'EOF' is received, it simply yields ().
	step EOF = yield () EOF
	
	-- When some chunks are received from the Enumeratee, they're written
	-- to the handle. Any exceptions are caught and reported, as in
	-- 'enumHandle'.
	step (Chunks bytes) = do
		eitherErr <- liftIO (E.try (mapM_ (B.hPut h) bytes))
		case eitherErr of
			Left err -> throwError (err :: E.SomeException)
			_ -> continue step

main :: IO ()
main = do
	-- Our example enumlates standard /bin/cat, where if the argument list
	-- is empty, data is echoed from stdin.
	args <- getArgs
	let enum = if null args
		then enumHandle 1 stdin
		else concatEnums (Prelude.map enumFile args)
	
	-- 'run' sends an EOF to an iteratee and returns its output, which
	-- is either a 'Yield' or an 'Error'.
	res <- run (enum $$ iterHandle stdout)
	
	-- Finally, 'run' has returned either an error or the iteratee's
	-- result. 'iterHandle' doesn't return a useful result, so as long
	-- as it succeeded the actual value is ignored.
	case res of
		Left err -> putStrLn $ "ERROR: " ++ show err
		Right _ -> return ()
