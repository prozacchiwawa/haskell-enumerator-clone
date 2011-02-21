-- Copyright (C) 2010-2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main where

import Criterion.Types
import qualified Criterion.Config as C
import qualified Criterion.Main as C
import qualified Progression.Config as P
import qualified Progression.Main as P

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text as T
import qualified Data.Text as TL

import Data.Enumerator hiding (map, replicate)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET

import Control.DeepSeq
import Data.Functor.Identity
import System.Environment
import System.Exit
import System.IO

instance NFData B.ByteString

instance NFData BL.ByteString where
	rnf a = rnf (BL.toChunks a)

bytes_100 :: B.ByteString
bytes_100 = B.replicate 100 0x61

chars_100 :: T.Text
chars_100 = T.replicate 100 (T.singleton 'a')

bench_binary :: Iteratee B.ByteString Identity b -> b
bench_binary iter = runIdentity (run_ (enum $$ iter)) where
	enum = enumList 2 (replicate 1000 bytes_100)

bench_text :: Iteratee T.Text Identity b -> b
bench_text iter = runIdentity (run_ (enum $$ iter)) where
	enum = enumList 2 (replicate 1000 chars_100)

bench_bind :: Iteratee Int Identity b -> b
bench_bind iter = runIdentity (run_ (enum 10000 $$ iter)) where
	enum 0 step = returnI step
	enum n (Continue k) = k (Chunks [n]) >>== enum (n - 1)
	enum _ step = returnI step

bench_enumFile :: Maybe Integer -> Iteratee B.ByteString IO b -> IO b
bench_enumFile limit iter = run_ (EB.enumFileRange "/dev/zero" Nothing limit $$ iter)

iterUnit :: Monad m => Iteratee a m ()
iterUnit = continue loop where
	loop EOF = yield () EOF
	loop (Chunks _) = continue loop

iterUnitTo :: Monad m => Int -> Iteratee a m ()
iterUnitTo n | n <= 0 = yield () EOF
iterUnitTo n = continue check where
	check EOF = yield () EOF
	check (Chunks _) = iterUnitTo (n - 1)

benchmarks :: [Benchmark]
benchmarks =
	[ bgroup "general"
	  [ bench "bind" (nf bench_bind iterUnit)
	  ]
	, bgroup "binary"
	  [ bench "takeWhile" (nf bench_binary (EB.takeWhile (const True)))
	  , bench "consume" (nf bench_binary EB.consume)
	  , bench "enumFile-nolimit" (nfIO (bench_enumFile Nothing (iterUnitTo 10000)))
	  , bench "enumFile-limit" (nfIO (bench_enumFile (Just 1000000000) (iterUnitTo 10000)))
	  ]
	, bgroup "text"
	  [ bench "takeWhile" (nf bench_text (ET.takeWhile (const True)))
	  , bench "consume" (nf bench_text ET.consume)
	  ]
	]

main :: IO ()
main = do
	args <- getArgs
	case args of
		"progression":extra -> withArgs extra $ P.defaultMain (bgroup "all" benchmarks)
		"criterion":extra -> withArgs extra $ let
			config = C.defaultConfig { C.cfgPerformGC = C.ljust True }
			in C.defaultMainWith config (return ()) benchmarks
		_ -> do
			name <- getProgName
			hPutStrLn stderr $ concat ["Usage: ", name, " <progression|criterion>"]
			exitFailure
