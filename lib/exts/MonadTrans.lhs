\h1{MonadTrans}
\haskell:module{
  \Name{MonadTrans}
  \Version{0.2}
  \Description{
	Declaration of \haskell:expr{MonadTrans} class.}
  \License{
	The Haskell Monad Template Library is Copyright &copy;
	Andy Gill, and the Oregon Graduate Institute of Science and
	Technology, 1999, All rights reserved, and is distributed as
	free software under the license in the file "License", which
	is included in the distribution.}
  \Author{
	Rendered by \A[HREF="http://www.cse.ogi.edu/~andy"]{Andy Gill},
	inspired by the paper
	\em{Functional Programming with Overloading and
	    Higher-Order Polymorphism},
	  \A[HREF="http://www.cse.ogi.edu/~mpj"]{Mark P Jones},
		Advanced School of Functional Programming, 1995.}
  \Restrictions{
	This works will all Haskell 98 compilers.
  }
  \Tested{Hugs98, GHC 4.03}
}
\begin{code}
module MonadTrans where
\end{code}

\begin{code}
import IO
\end{code}

\haskell:class{
  \Purpose{Monad to facilitate stackable Monads}
  \Description{
	Provides a way of digging into an outer
	monad, giving access to (lifting) the inner monad.}
}

\begin{code}
class MonadTrans t where
	lift :: Monad m => m a -> t m a
\end{code}

\begin{code}
class (Monad m) => MonadIO m where
	liftIO :: IO a -> m a

instance MonadIO IO where
	liftIO = id
\end{code}
