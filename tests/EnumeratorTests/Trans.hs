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
	, test_RunRWS
	, test_EvalRWS
	, test_ExecRWS
	]

test_RunIdentity :: Suite
test_RunIdentity = assertions "runIdentity" $ do
	$expect $ equal
		(['a'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runIdentity (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runIdentity (E.throwError (ErrorCall "err")))

test_RunMaybe :: Suite
test_RunMaybe = assertions "runMaybe" $ do
	$expect $ equal
		(Just ['a'], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runMaybe (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Nothing :: Maybe [Char], ['a', 'b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runMaybe (lift mzero)
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runMaybe (E.throwError (ErrorCall "err")))

test_RunError :: Suite
test_RunError = assertions "runError" $ do
	$expect $ equal
		(Right ['a'] :: Either String [Char], ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runError (EL.take 1)
			extra <- EL.consume
			return (x, extra))
	$expect $ equal
		(Left "err" :: Either String [Char], ['a', 'b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runError (lift (ErrorT.throwError "err"))
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runError $ do
			_ <- E.throwError (ErrorCall "err")
			lift (ErrorT.throwError ("err2" :: String)))

test_RunReader :: Suite
test_RunReader = assertions "runReader" $ do
	$expect $ equal
		((['a'], 'A'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runReader 'A' $ do
				x <- EL.take 1
				y <- lift ReaderT.ask
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runReader 'A' (E.throwError (ErrorCall "err")))

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
			xy <- ET.runState 'A' $ do
				x <- EL.take 1
				y <- lift StateT_L.get
				lift (StateT_L.put 'B')
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runState 'A' (E.throwError (ErrorCall "err")))

test_RunState_Strict :: Suite
test_RunState_Strict = assertions "strict" $ do
	$expect $ equal
		(((['a'], 'A'), 'B'), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runState' 'A' $ do
				x <- EL.take 1
				y <- lift StateT_S.get
				lift (StateT_S.put 'B')
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runState' 'A' (E.throwError (ErrorCall "err")))

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
			xy <- ET.evalState 'A' $ do
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
			xy <- ET.evalState' 'A' $ do
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
			x <- ET.runWriter $ do
				lift (WriterT_L.tell ['A'])
				x <- EL.take 1
				lift (WriterT_L.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runWriter $ do
			_ <- E.throwError (ErrorCall "err")
			lift (WriterT_L.tell ['A']))

test_RunWriter_Strict :: Suite
test_RunWriter_Strict = assertions "strict" $ do
	$expect $ equal
		((['a'], ['A', 'B']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			x <- ET.runWriter' $ do
				lift (WriterT_S.tell ['A'])
				x <- EL.take 1
				lift (WriterT_S.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))
	$expect $ equalExc
		(ErrorCall "err")
		(E.runLists [] $ ET.runWriter' $ do
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
			x <- ET.execWriter $ do
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
			x <- ET.execWriter' $ do
				lift (WriterT_S.tell ['A'])
				x <- EL.take 1
				lift (WriterT_S.tell ['B'])
				return x
			extra <- EL.consume
			return (x, extra))

test_RunRWS :: Suite
test_RunRWS = suite "runRWSI"
	[ test_RunRWSI_Lazy
	, test_RunRWSI_Strict
	]

test_RunRWSI_Lazy :: Suite
test_RunRWSI_Lazy = assertions "lazy" $ do
	$expect $ equal
		(((['a'], 'A'), 'B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runRWS 'A' 'A' $ do
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
		(E.runLists [] $ ET.runRWS 'A' 'A' $ do
			_ <- E.throwError (ErrorCall "err")
			lift (RWST_L.tell ['Y']))

test_RunRWSI_Strict :: Suite
test_RunRWSI_Strict = assertions "strict" $ do
	$expect $ equal
		(((['a'], 'A'), 'B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.runRWS' 'A' 'A' $ do
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
		(E.runLists [] $ ET.runRWS' 'A' 'A' $ do
			_ <- E.throwError (ErrorCall "err")
			lift (RWST_S.tell ['Y']))

test_EvalRWS :: Suite
test_EvalRWS = suite "evalRWS"
	[ test_EvalRWS_Lazy
	, test_EvalRWS_Strict
	]

test_EvalRWS_Lazy :: Suite
test_EvalRWS_Lazy = assertions "lazy" $ do
	$expect $ equal
		(((['a'], 'A'), ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.evalRWS 'A' 'Z' $ do
				lift (RWST_L.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_L.ask
				z <- lift RWST_L.get
				lift (RWST_L.tell [z])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_EvalRWS_Strict :: Suite
test_EvalRWS_Strict = assertions "strict" $ do
	$expect $ equal
		(((['a'], 'A'), ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.evalRWS' 'A' 'Z' $ do
				lift (RWST_S.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_S.ask
				z <- lift RWST_S.get
				lift (RWST_S.tell [z])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_ExecRWS :: Suite
test_ExecRWS = suite "execRWS"
	[ test_ExecRWS_Lazy
	, test_ExecRWS_Strict
	]

test_ExecRWS_Lazy :: Suite
test_ExecRWS_Lazy = assertions "lazy" $ do
	$expect $ equal
		(('B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.execRWS 'Z' 'A' $ do
				lift (RWST_L.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_L.ask
				lift (RWST_L.modify succ)
				lift (RWST_L.tell [y])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))

test_ExecRWS_Strict :: Suite
test_ExecRWS_Strict = assertions "strict" $ do
	$expect $ equal
		(('B', ['Y', 'Z']), ['b'])
		(E.runLists_ [['a'], ['b']] $ do
			xy <- ET.execRWS' 'Z' 'A' $ do
				lift (RWST_S.tell ['Y'])
				x <- EL.take 1
				y <- lift RWST_S.ask
				lift (RWST_S.modify succ)
				lift (RWST_S.tell [y])
				return (x, y)
			extra <- EL.consume
			return (xy, extra))
