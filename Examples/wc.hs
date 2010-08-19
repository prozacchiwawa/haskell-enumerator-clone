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
import Data.Word (Word8)
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
	step acc (Chunks xs) = continue $ step (foldl' foldStep acc xs)
	foldStep acc bytes = acc + toInteger (B.length bytes)

-- iterLines is similar, except it only counts newlines ('\n')

iterLines :: Monad m => Iteratee e B.ByteString m Integer
iterLines = continue (step 0) where
	step acc EOF = yield acc EOF
	step acc (Chunks xs) = continue $ step (foldl' foldStep acc xs)
	foldStep acc bytes = acc + countChar '\n' bytes
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
			Just accT' -> continue $ step accT'
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
		eitherStat <- run (iter >>== enumFile filename)
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
		let (x0, x1, x2, x3) = indexes bytes
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
	
	indexes bytes = (x0, x1, x2, x3) where
		x0 = B.index bytes 0
		x1 = B.index bytes 1
		x2 = B.index bytes 2
		x3 = B.index bytes 3

-- UTF8 decoding gunk; mostly copied from Data.Text

between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}

validate1    :: Word8 -> Bool
validate1 x1 = between x1 0x00 0x7F
{-# INLINE validate1 #-}

validate2       :: Word8 -> Word8 -> Bool
validate2 x1 x2 = between x1 0xC2 0xDF && between x2 0x80 0xBF
{-# INLINE validate2 #-}

validate3          :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3 #-}
validate3 x1 x2 x3 = validate3_1 ||
                     validate3_2 ||
                     validate3_3 ||
                     validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4             :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 = validate4_1 ||
                        validate4_2 ||
                        validate4_3
  where 
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF

