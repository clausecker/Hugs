\h1{MonadReader}
\haskell:module{
  \Name{MonadReader}
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
	This requires multi parameter classes and fundeps.
	}
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module MonadReader (
	MonadReader(..),
	asks,
	Reader(..),
	runReader,
	mapReader,
	withReader,
	ReaderT(..),
	runReaderT,
	mapReaderT,
	withReaderT,
	module Monad,
	module MonadFix,
	module MonadTrans,
	) where
\end{code}

\begin{code}
import Monad
import MonadFix
import MonadTrans
\end{code}

\haskell:class{
  \Name{MonadReader}
  \ask{asks for the internal (non-mutable) state.}
}

\begin{code}
class (Monad m) => MonadReader r m | m -> r where
	ask   :: m r
	local :: (r -> r) -> m a -> m a
\end{code}

This allows you to provide a projection function.

\begin{code}
asks :: (MonadReader r m) => (r -> a) -> m a
asks f = do
	r <- ask
	return (f r)
\end{code}

\h2{The partially applied function type is a simple reader monad}

\begin{code}
instance Functor ((->) r) where
	fmap = (.)

instance Monad ((->) r) where
	return  = const
	m >>= k = \r -> k (m r) r

instance MonadFix ((->) r) where
	mfix f = \r -> let a = f a r in a

instance MonadReader r ((->) r) where
	ask       = id
	local f m = m . f
\end{code}

\h2{Our parameterizable reader monad}

\begin{code}
newtype Reader r a = Reader { runReader :: r -> a }
\end{code}

\begin{code}
instance Functor (Reader r) where
	fmap f m = Reader $ \r -> f (runReader m r)

instance Monad (Reader r) where
	return a = Reader $ \_ -> a
	m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance MonadFix (Reader r) where
	mfix f = Reader $ \r -> let a = runReader (f a) r in a

instance MonadReader r (Reader r) where
	ask       = Reader id
	local f m = Reader $ runReader m . f
\end{code}

\begin{code}
mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m
\end{code}

This is a more general version of local.

\begin{code}
withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f
\end{code}

\h2{Our parameterizable reader monad, with an inner monad}

\begin{code}
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
\end{code}

\begin{code}
instance (Monad m) => Functor (ReaderT r m) where
	fmap f m = ReaderT $ \r -> do
		a <- runReaderT m r
		return (f a)

instance (Monad m) => Monad (ReaderT r m) where
	return a = ReaderT $ \_ -> return a
	m >>= k  = ReaderT $ \r -> do
		a <- runReaderT m r
		runReaderT (k a) r
	fail msg = ReaderT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
	mzero       = ReaderT $ \_ -> mzero
	m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
	mfix f = ReaderT $ \r -> mfix $ \a -> runReaderT (f a) r

instance (Monad m) => MonadReader r (ReaderT r m) where
	ask       = ReaderT return
	local f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadTrans (ReaderT r) where
	lift m = ReaderT $ \_ -> m

instance (MonadIO m) => MonadIO (ReaderT r m) where
	liftIO = lift . liftIO
\end{code}

\begin{code}
mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f
\end{code}
