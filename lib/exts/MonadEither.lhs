\h1{MonadEither}
\haskell:module{
  \Name{MonadEither}
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
  \Restrictions{
	This requires the ability to overload at complex
	types.
	}
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module MonadEither (Error(..)) where

import MonadError
\end{code}
