{-# LANGUAGE CPP #-}
module Data.Enumerator.Util where

import           Data.Char (toUpper, intToDigit, ord)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Word (Word8)
import           Numeric (showIntAtBase)

pad0 :: Int -> String -> String
pad0 size str = padded where
	len = Prelude.length str
	padded = if len >= size
		then str
		else Prelude.replicate (size - len) '0' ++ str

reprChar :: Char -> String
reprChar c = "U+" ++ (pad0 4 (showIntAtBase 16 (toUpper . intToDigit) (ord c) ""))

reprWord :: Word8 -> String
reprWord w = "0x" ++ (pad0 2 (showIntAtBase 16 (toUpper . intToDigit) w ""))

tSpanBy  :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
tlSpanBy :: (Char -> Bool) -> TL.Text -> (TL.Text, TL.Text)
#if MIN_VERSION_text(0,11,0)
tSpanBy = T.span
tlSpanBy = TL.span
#else
tSpanBy = T.spanBy
tlSpanBy = TL.spanBy
#endif

textToStrict :: TL.Text -> T.Text
#if MIN_VERSION_text(0,8,0)
textToStrict = TL.toStrict
#else
textToStrict = T.concat . TL.toChunks
#endif
