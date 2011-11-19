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
	[ test_RunIdentity
	, test_RunMaybe
	, test_RunError
	, test_RunReader
	, test_RunState
	, test_EvalState
	, test_RunWriter
	, test_ExecWriter
	, test_RunRWSI
	, test_EvalRWSI
	, test_ExecRWSI
	]

test_RunIdentity :: Suite
test_RunIdentity = assertions "runIdentity" $ do
	$expect $ equal
		(['a'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runIdentityI (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runIdentityI (E.throwError (ErrorCall "err")))

test_RunMaybe :: Suite
test_RunMaybe = assertions "runMaybe" $ do
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

test_RunError :: Suite
test_RunError = assertions "runError" $ do
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

test_RunReader :: Suite
test_RunReader = assertions "runReader" $ do
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

test_RunState :: Suite
test_RunState = suite "runState"
	[ test_RunState_Lazy
	, test_RunState_Strict
	]

test_RunState_Lazy :: Suite
test_RunState_Lazy = assertions "lazy" $ do
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

test_RunState_Strict :: Suite
test_RunState_Strict = assertions "strict" $ do
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

test_EvalState :: Suite
test_EvalState = suite "evalState"
	[ test_EvalState_Lazy
	, test_EvalState_Strict
	]

test_EvalState_Lazy :: Suite
test_EvalState_Lazy = assertions "lazy" $ do
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

test_EvalState_Strict :: Suite
test_EvalState_Strict = assertions "strict" $ do
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

test_RunWriter :: Suite
test_RunWriter = suite "runWriter"
	[ test_RunWriter_Lazy
	, test_RunWriter_Strict
	]

test_RunWriter_Lazy :: Suite
test_RunWriter_Lazy = assertions "lazy" $ do
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

test_RunWriter_Strict :: Suite
test_RunWriter_Strict = assertions "strict" $ do
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

test_ExecWriter :: Suite
test_ExecWriter = suite "execWriter"
	[ test_ExecWriter_Lazy
	, test_ExecWriter_Strict
	]

test_ExecWriter_Lazy :: Suite
test_ExecWriter_Lazy = assertions "lazy" $ do
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

test_ExecWriter_Strict :: Suite
test_ExecWriter_Strict = assertions "strict" $ do
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
