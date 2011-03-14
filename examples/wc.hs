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
import Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import qualified Data.ByteString as B

-- support imports
import Control.Monad (unless, forM_)
import System.IO
import System.Console.GetOpt
import System.Environment
import System.Exit

-- support wc modes -c (bytes), -m (characters), and -l (lines)

-- iterBytes simply counts how many bytes are in each chunk, accumulates this
-- count, and returns it when EOF is received.

iterBytes :: Monad m => Iteratee B.ByteString m Integer
iterBytes = EB.fold (\acc _ -> acc + 1) 0

-- iterLines is similar, except it only counts newlines ('\n')

iterLines :: Monad m => Iteratee B.ByteString m Integer
iterLines = EB.fold step 0 where
	step acc 0xA = acc + 1
	step acc _ = acc

-- iterChars is a bit more complicated. It has to decode the input (for now,
-- assuming UTF-8) before performing any counting. Leftover bytes, not part
-- of a valid UTF-8 character, are yielded as surplus
--
-- Note the use of joinI. 'ET.decode' is an enumeratee, which means it returns
-- an iteratee yielding an inner step. 'joinI' "collapses" an enumeratee's
-- return value, much as 'join' does to monadic values.

iterChars :: Monad m => Iteratee B.ByteString m Integer
iterChars = joinI (ET.decode ET.utf8 $$ count) where
	count = ET.fold (\acc _ -> acc + 1) 0

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
		putStr $ filename ++ ": "
		
		eitherStat <- run (EB.enumFile filename $$ iter)
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
