\h1{MonadFix}
\haskell:module{
  \Name{MonadFix}
  \Version{0.2}
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
  \Tested{Hugs98, GHC 4.05}
  \Restrictions{
	Requires IOExts and ST.
	}
}
\begin{code}
module MonadFix where

import IOExts
import ST
\end{code}

\begin{code}
fix :: (a -> a) -> a
fix f = let x = f x in x
\end{code}

\begin{code}
class (Monad m) => MonadFix m where
	mfix :: (a -> m a) -> m a
\end{code}

\begin{code}
-- Perhaps these should live beside (the ST & IO) definition.
instance MonadFix IO where
	mfix = fixIO

instance MonadFix (ST s) where
	mfix = fixST

instance MonadFix Maybe where
	mfix f = let
		a = f $ case a of
			Just x -> x
			_      -> error "empty mfix argument"
		in a
\end{code}
