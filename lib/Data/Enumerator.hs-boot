module Data.Enumerator where
import qualified Control.Exception as Exc
data Stream a
data Step a m b
	= Continue (Stream a -> Iteratee a m b)
	| Yield b (Stream a)
	| Error Exc.SomeException
newtype Iteratee a m b = Iteratee
	{ runIteratee :: m (Step a m b)
	}
type Enumerator a m b = Step a m b -> Iteratee a m b
type Enumeratee ao ai m b = Step ai m b -> Iteratee ao m (Step ai m b)
