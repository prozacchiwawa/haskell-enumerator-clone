{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module EnumeratorTests.Trans
	( test_Trans
	) where

import           Control.Exception
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Error as ErrorT
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.State.Lazy as StateT_L
import qualified Control.Monad.Trans.State.Strict as StateT_S
import qualified Control.Monad.Trans.Writer.Lazy as WriterT_L
import qualified Control.Monad.Trans.Writer.Strict as WriterT_S
import qualified Control.Monad.Trans.RWS.Lazy as RWST_L
import qualified Control.Monad.Trans.RWS.Strict as RWST_S

import           Test.Chell

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Trans as ET

import           EnumeratorTests.Util (equalExc)

test_Trans :: Suite
test_Trans = suite "transformers"
	[ test_RunIdentityI
	, test_RunMaybeI
	, test_RunErrorI
	, test_RunReaderI
	, test_RunStateI
	, test_EvalStateI
	, test_RunWriterI
	, test_ExecWriterI
	, test_RunRWSI
	, test_EvalRWSI
	, test_ExecRWSI
	]

test_RunIdentityI :: Suite
test_RunIdentityI = assertions "runIdentityI" $ do
	$expect $ equal
		(['a'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runIdentityI (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runIdentityI (E.throwError (ErrorCall "err")))

test_RunMaybeI :: Suite
test_RunMaybeI = assertions "runMaybeI" $ do
	$expect $ equal
		(Just ['a'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runMaybeI (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Nothing :: Maybe [Char], ['a', 'b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runMaybeI (lift mzero)
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runMaybeI (E.throwError (ErrorCall "err")))

test_RunErrorI :: Suite
test_RunErrorI = assertions "runErrorI" $ do
	$expect $ equal
		(Right ['a'] :: Either String [Char], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runErrorI (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Left "err" :: Either String [Char], ['a', 'b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runErrorI (lift (ErrorT.throwError "err"))
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runErrorI $ do
			_ <- E.throwError (ErrorCall "err")
			lift (ErrorT.throwError ("err2" :: String)))

test_RunReaderI :: Suite
test_RunReaderI = assertions "runReaderI" $ do
	$expect $ equal
		((['a'], 'A'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runReaderI 'A' $ do
				x <- EL.take 1
				y <- lift ReaderT.ask
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runReaderI 'A' (E.throwError (ErrorCall "err")))

test_RunStateI :: Suite
test_RunStateI = suite "runStateI"
	[ test_RunStateI_Lazy
	, test_RunStateI_Strict
	]

test_RunStateI_Lazy :: Suite
test_RunStateI_Lazy = assertions "lazy" $ do
	$expect $ equal
		(((['a'], 'A'), 'B'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runStateI 'A' $ do
				x <- EL.take 1
				y <- lift StateT_L.get
				lift (StateT_L.put 'B')
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runStateI 'A' (E.throwError (ErrorCall "err")))

test_RunStateI_Strict :: Suite
test_RunStateI_Strict = assertions "strict" $ do
	$expect $ equal
		(((['a'], 'A'), 'B'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runStateI' 'A' $ do
				x <- EL.take 1
				y <- lift StateT_S.get
				lift (StateT_S.put 'B')
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runStateI' 'A' (E.throwError (ErrorCall "err")))

test_EvalStateI :: Suite
test_EvalStateI = suite "evalStateI"
	[ test_EvalStateI_Lazy
	, test_EvalStateI_Strict
	]

test_EvalStateI_Lazy :: Suite
test_EvalStateI_Lazy = assertions "lazy" $ do
	$expect $ equal
		((['a'], 'A'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.evalStateI 'A' $ do
				x <- EL.take 1
				y <- lift StateT_L.get
				lift (StateT_L.put 'B')
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_EvalStateI_Strict :: Suite
test_EvalStateI_Strict = assertions "strict" $ do
	$expect $ equal
		((['a'], 'A'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.evalStateI' 'A' $ do
				x <- EL.take 1
				y <- lift StateT_S.get
				lift (StateT_S.put 'B')
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_RunWriterI :: Suite
test_RunWriterI = suite "runWriterI"
	[ test_RunWriterI_Lazy
	, test_RunWriterI_Strict
	]

test_RunWriterI_Lazy :: Suite
test_RunWriterI_Lazy = assertions "lazy" $ do
	$expect $ equal
		((['a'], ['A', 'B']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runWriterI $ do
				lift (WriterT_L.tell ['A'])
				x <- EL.take 1
				lift (WriterT_L.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runWriterI $ do
			_ <- E.throwError (ErrorCall "err")
			lift (WriterT_L.tell ['A']))

test_RunWriterI_Strict :: Suite
test_RunWriterI_Strict = assertions "strict" $ do
	$expect $ equal
		((['a'], ['A', 'B']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runWriterI' $ do
				lift (WriterT_S.tell ['A'])
				x <- EL.take 1
				lift (WriterT_S.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runWriterI' $ do
			_ <- E.throwError (ErrorCall "err")
			lift (WriterT_S.tell ['A']))

test_ExecWriterI :: Suite
test_ExecWriterI = suite "execWriterI"
	[ test_ExecWriterI_Lazy
	, test_ExecWriterI_Strict
	]

test_ExecWriterI_Lazy :: Suite
test_ExecWriterI_Lazy = assertions "lazy" $ do
	$expect $ equal
		(['A', 'B'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.execWriterI $ do
				lift (WriterT_L.tell ['A'])
				x <- EL.take 1
				lift (WriterT_L.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))

test_ExecWriterI_Strict :: Suite
test_ExecWriterI_Strict = assertions "strict" $ do
	$expect $ equal
		(['A', 'B'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.execWriterI' $ do
				lift (WriterT_S.tell ['A'])
				x <- EL.take 1
				lift (WriterT_S.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))

test_RunRWSI :: Suite
test_RunRWSI = suite "runRWSI"
	[ test_RunRWSI_Lazy
	, test_RunRWSI_Strict
	]

test_RunRWSI_Lazy :: Suite
test_RunRWSI_Lazy = assertions "lazy" $ do
	$expect $ equal
		(((['a'], 'A'), 'B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runRWSI 'A' 'A' $ do
				lift (RWST_L.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_L.ask
				lift (RWST_L.modify succ)
				lift (RWST_L.tell ['Z'])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runRWSI 'A' 'A' $ do
			_ <- E.throwError (ErrorCall "err")
			lift (RWST_L.tell ['Y']))

test_RunRWSI_Strict :: Suite
test_RunRWSI_Strict = assertions "strict" $ do
	$expect $ equal
		(((['a'], 'A'), 'B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runRWSI' 'A' 'A' $ do
				lift (RWST_S.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_S.ask
				lift (RWST_S.modify succ)
				lift (RWST_S.tell ['Z'])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runRWSI' 'A' 'A' $ do
			_ <- E.throwError (ErrorCall "err")
			lift (RWST_S.tell ['Y']))

test_EvalRWSI :: Suite
test_EvalRWSI = suite "evalRWSI"
	[ test_EvalRWSI_Lazy
	, test_EvalRWSI_Strict
	]

test_EvalRWSI_Lazy :: Suite
test_EvalRWSI_Lazy = assertions "lazy" $ do
	$expect $ equal
		(((['a'], 'A'), ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.evalRWSI 'A' 'Z' $ do
				lift (RWST_L.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_L.ask
				z <- lift RWST_L.get
				lift (RWST_L.tell [z])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_EvalRWSI_Strict :: Suite
test_EvalRWSI_Strict = assertions "strict" $ do
	$expect $ equal
		(((['a'], 'A'), ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.evalRWSI' 'A' 'Z' $ do
				lift (RWST_S.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_S.ask
				z <- lift RWST_S.get
				lift (RWST_S.tell [z])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_ExecRWSI :: Suite
test_ExecRWSI = suite "execRWSI"
	[ test_ExecRWSI_Lazy
	, test_ExecRWSI_Strict
	]

test_ExecRWSI_Lazy :: Suite
test_ExecRWSI_Lazy = assertions "lazy" $ do
	$expect $ equal
		(('B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.execRWSI 'Z' 'A' $ do
				lift (RWST_L.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_L.ask
				lift (RWST_L.modify succ)
				lift (RWST_L.tell [y])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_ExecRWSI_Strict :: Suite
test_ExecRWSI_Strict = assertions "strict" $ do
	$expect $ equal
		(('B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.execRWSI' 'Z' 'A' $ do
				lift (RWST_S.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_S.ask
				lift (RWST_S.modify succ)
				lift (RWST_S.tell [y])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
