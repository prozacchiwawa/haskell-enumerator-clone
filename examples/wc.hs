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

import           Control.Monad (unless, forM_)
import           Data.ByteString (ByteString)
import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit

import           Data.Enumerator as E
import qualified Data.Enumerator.Binary as Binary
import qualified Data.Enumerator.Text as Text

-- support wc modes -c (bytes), -m (characters), and -l (lines)

-- iterBytes simply counts how many bytes are in each chunk, accumulates this
-- count, and returns it when EOF is received.

iterBytes :: Monad m => Iteratee ByteString m Integer
iterBytes = Binary.fold (\acc _ -> acc + 1) 0

-- iterLines is similar, except it only counts newlines ('\n')

iterLines :: Monad m => Iteratee ByteString m Integer
iterLines = Binary.fold step 0 where
	step acc 0xA = acc + 1
	step acc _ = acc

-- iterChars is a bit more complicated. It has to decode the input (for now,
-- assuming UTF-8) before performing any counting. Leftover bytes, not part
-- of a valid UTF-8 character, are yielded as surplus
--
-- Note the use of (=$). This lets an enumeratee send data directly to an
-- iteratee, without worrying about leftover input.

iterChars :: Monad m => Iteratee ByteString m Integer
iterChars = Text.decode Text.utf8 =$ Text.fold (\acc _ -> acc + 1) 0

main :: IO ()
main = do
	(mode, files) <- getMode
	
	-- Exactly matching wc's output is too annoying, so this example
	-- will just print one line per file, and support counting at most
	-- one statistic per run
	let iter = case mode of
		OptionBytes -> iterBytes
		OptionLines -> iterLines
		OptionChars -> iterChars
	
	forM_ files $ \filename -> do
		putStr (filename ++ ": ")
		
		eitherStat <- run (Binary.enumFile filename $$ iter)
		putStrLn $ case eitherStat of
			Left err -> "ERROR: " ++ show err
			Right stat -> show stat

-- uninteresting option parsing follows

data Option
	= OptionBytes
	| OptionChars
	| OptionLines

optionInfo :: [OptDescr Option]
optionInfo =
	[ Option ['c'] ["bytes"] (NoArg OptionBytes) "count bytes"
	, Option ['m'] ["chars"] (NoArg OptionChars) "count characters"
	, Option ['l'] ["lines"] (NoArg OptionLines) "count lines"
	]

usage :: String -> String
usage name = "Usage: " ++ name ++ " <MODE> [FILES]"

getMode :: IO (Option, [FilePath])
getMode = do
	args <- getArgs
	let (options, files, errors) = getOpt Permute optionInfo args
	unless (null errors && not (null options) && not (null files)) $ do
		name <- getProgName
		hPutStrLn stderr $ concat errors
		hPutStrLn stderr $ usageInfo (usage name) optionInfo
		exitFailure
	
	return (Prelude.head options, files)
