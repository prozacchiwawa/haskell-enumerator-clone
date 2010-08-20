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
import Data.Enumerator
import qualified Data.ByteString as B
import qualified Foreign as F
import System.IO
import System.Environment (getArgs)

-- The following definitions of 'enumHandle', 'enumFile', and 'iterHandle' are
-- copied from "Data.Enumerator.IO", with additional comments so they're easier
-- to understand.

enumHandle :: Integer -- ^ Buffer size
           -> Handle
           -> Enumerator SomeException B.ByteString IO b
enumHandle bufferSize h = Iteratee . io where
	intSize = fromInteger bufferSize
	
	-- Allocate a single buffer for reading from the file
	io step = F.allocaBytes intSize $ \p -> loop step p
	
	-- If more input is required before the enumerator's iteratee can
	-- yield a result, feed it from the handle. If a different step is
	-- received ('Error' or 'Yield'), just pass it through.
	loop (Continue k) = read' k
	loop step = const $ return step
	
	read' k p = do
		-- While not strictly necessary to proper operation, catching
		-- exceptions here allows more unified exception handling when
		-- the enumerator/iteratee is run.
		eitherN <- E.try $ hGetBuf h p intSize
		case eitherN of
			Left err -> return $ Error err
			
			-- if 'hGetBuf' returns 0, then the handle has reached
			-- EOF. The enumerator has two choices:
			--
			-- * Send EOF to its iteratee, and return the result
			-- * Return a Continue, with the current continuation
			--
			-- The second is better, because it allows enumerators
			-- to be composed with (>>==). If EOF is sent, only
			-- one enumerator can be read at a time.
			Right 0 -> return $ Continue k
			
			-- 'hGetBuf' was at least partially successful, so read
			-- bytes into a ByteString and pass it through to the
			-- iteratee.
			Right n -> do
				bytes <- B.packCStringLen (p, n)
				step <- runIteratee (k (Chunks [bytes]))
				loop step p

enumFile :: FilePath -> Enumerator E.SomeException B.ByteString IO b
enumFile path s = Iteratee $ do
	-- Opening the file can be performed either inside or outside of the
	-- Iteratee. Inside allows exceptions to be caught and propagated
	-- through the 'Error' step constructor.
	eitherH <- E.try $ openBinaryFile path ReadMode
	case eitherH of
		Left err -> return $ Error err
		Right h -> finally
			(runIteratee (enumHandle 4096 h s))
			(hClose h)

-- 'iterHandle' is the opposite of 'enumHandle', in that it *writes to* a
-- handle instead of reading from it. An enumerator is a source, an iteratee
-- is a sink.
iterHandle :: Handle -> Iteratee SomeException B.ByteString IO ()

-- Most iteratees start in the 'Continue' state, as they need some
-- input before they can produce any value.
iterHandle h = continue step where
	
	-- This iteratee produces no value; its only purpose is its
	-- side-effects. When 'EOF' is received, it simply yields ().
	step EOF = yield () EOF
	
	-- When some chunks are received from the Enumeratee, they're written
	-- to the handle. Any exceptions are caught and reported, as in
	-- 'enumHandle'.
	step (Chunks bytes) = Iteratee $ do
		eitherErr <- E.try $ mapM_ (B.hPut h) bytes
		return $ case eitherErr of
			Left err -> Error err
			_ -> Continue step

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
