\h1{Monoid}
\haskell:module{
  \Name{Monoid}
  \Version{0.2}
  \Description{
	Declaration of the \haskell:expr{Monoid} class,
	and instances for list and functions.}
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
	This only works with compilier than has extensions that
	can handle an instance for the type "\haskell:expr{a -> a}".
  }
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module Monoid where
\end{code}

\haskell:class{
  \Purpose{Definition of the Monoid class.}
  \Description{
	The Monoid class, as defined by
	an \haskell:expr{mempty} and a \haskell:expr{mappend}.
  }
}
\begin{code}
class Monoid a where
	mempty  :: a
	mappend :: a -> a -> a
	mconcat :: [a] -> a
\end{code}

Now the default for \haskell:expr{mconcat}.  For most types, this
default will be used, but is included in the class definition so
that optimized version of \haskell:expr{mconcat} can be provided
for specific types.

\begin{code}
	mconcat = foldr mappend mempty
\end{code}

Monoid instances.
\begin{code}
instance Monoid [a] where
	mempty  = []
	mappend = (++)

instance Monoid (a -> a) where
	mempty  = id
	mappend = (.)

instance Monoid () where
	-- Should it be strict?
	mempty        = ()
	_ `mappend` _ = ()
	mconcat _     = ()
\end{code}
