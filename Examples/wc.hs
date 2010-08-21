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
import Data.Enumerator.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- support imports
import Control.Exception as E
import Data.List
import Data.Bits ((.&.))
import Control.Monad (unless, forM_)
import System.IO
import System.Console.GetOpt
import System.Environment
import System.Exit

-- support wc modes -c (bytes), -m (characters), and -l (lines)

-- iterBytes simply counts how many bytes are in each chunk, accumulates this
-- count, and returns it when EOF is received

iterBytes :: Monad m => Iteratee e B.ByteString m Integer
iterBytes = continue (step 0) where
	step acc EOF = yield acc EOF
	step acc (Chunks xs) = continue $ step $! foldl' foldStep acc xs
	foldStep acc bytes = acc + toInteger (B.length bytes)

-- iterLines is similar, except it only counts newlines ('\n')
--
-- Because it's basically the same as 'iterBytes', we use it to demonstrate
-- the 'liftFoldL\'' helper function.

iterLines :: Monad m => Iteratee e B.ByteString m Integer
iterLines = liftFoldL' step 0 where
	step acc bytes = acc + countChar '\n' bytes
	countChar c = B8.foldl (\acc c' -> if c' == c then acc + 1 else acc) 0

-- iterChars is a bit more complicated. It has to decode the input (for now,
-- assuming UTF-8) before performing any counting. Leftover bytes, not part
-- of a valid UTF-8 character, are yielded as surplus
--
-- Since it's possible for the input file's encoding to be non-UTF8, a bit
-- of error handling is also required.

iterChars :: Monad m => Iteratee E.SomeException B.ByteString m Integer
iterChars = continue (step (B.empty, 0)) where
	step accT chunk = case chunk of
		EOF -> let (extra, acc) = accT in yield acc (Chunks [extra])
		(Chunks xs) -> case foldl' foldStep (Just accT) xs of
			Just accT' -> continue $ step $! accT'
			Nothing -> throwError (E.SomeException (E.ErrorCall "Invalid UTF-8"))
	
	-- The 'decodeUtf8' function is complicated, and defined later, but
	-- all it does is decode as much input as possible, then return any
	-- remaining bytes.
	
	foldStep Nothing _ = Nothing
	foldStep (Just (extra, acc)) bytes = case decodeUtf8 (B.append extra bytes) of
		Just (text, extra') -> Just (extra', acc + toInteger (T.length text))
		Nothing -> Nothing


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
		
		-- see cat.hs for commented implementation of 'Data.Enumerator.IO.enumFile'
		eitherStat <- run (enumFile filename $$ iter)
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

-- Incremental UTF-8 validator / decoder

decodeUtf8 :: B.ByteString -> Maybe (T.Text, B.ByteString)
decodeUtf8 allBytes = loop B.empty allBytes where
	
	loop acc bytes | B.null bytes = Just (T.decodeUtf8 acc, bytes)
	loop acc bytes = do
		let x0 = B.index bytes 0
		req <- required x0
		if req > B.length bytes
			then Just (T.decodeUtf8 acc, bytes)
			else loop (B.append acc (B.take req bytes)) (B.drop req bytes)
	
	required x0
		| x0 .&. 0x80 == 0x00 = Just 1
		| x0 .&. 0xE0 == 0xC0 = Just 2
		| x0 .&. 0xF0 == 0xE0 = Just 3
		| x0 .&. 0xF8 == 0xF0 = Just 4
		| otherwise           = Nothing
