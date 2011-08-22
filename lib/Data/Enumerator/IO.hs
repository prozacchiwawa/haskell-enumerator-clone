-- |
-- Module: Data.Enumerator.IO
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Deprecated in 0.4.5: use "Data.Enumerator.Binary" instead
module Data.Enumerator.IO
	{-# DEPRECATED "Use 'Data.Enumerator.Binary' instead" #-}
	( enumHandle
	, enumFile
	, iterHandle
	) where
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import qualified System.IO as IO

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

{-# DEPRECATED enumHandle "Use 'Data.Enumerator.Binary.enumHandle' instead" #-}
-- | Deprecated in 0.4.5: use 'EB.enumHandle' instead
enumHandle :: MonadIO m
           => Integer
           -> IO.Handle
           -> E.Enumerator B.ByteString m b
enumHandle = EB.enumHandle

{-# DEPRECATED enumFile "Use 'Data.Enumerator.Binary.enumFile' instead" #-}
-- | Deprecated in 0.4.5: use 'EB.enumFile' instead
enumFile :: FilePath -> E.Enumerator B.ByteString IO b
enumFile = EB.enumFile

{-# DEPRECATED iterHandle "Use 'Data.Enumerator.Binary.iterHandle' instead" #-}
-- | Deprecated in 0.4.5: use 'EB.iterHandle' instead
iterHandle :: MonadIO m => IO.Handle
           -> E.Iteratee B.ByteString m ()
iterHandle = EB.iterHandle
