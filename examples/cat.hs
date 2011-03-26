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
import Data.Enumerator
import Data.Enumerator.Binary (enumFile, enumHandle, iterHandle)
import System.IO (stdin, stdout)
import System.Environment (getArgs)

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
