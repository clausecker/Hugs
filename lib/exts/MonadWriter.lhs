\h1{MonadWriter}
\haskell:module{
  \Name{MonadWriter}
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
module MonadWriter (
	MonadWriter(..),
	listens,
	censor,
	Writer(..),
	runWriter,
	execWriter,
	mapWriter,
	WriterT(..),
	runWriterT,
	execWriterT,
	mapWriterT,
	module Monad,
	module Monoid,
	module MonadFix,
	module MonadTrans,
	) where
\end{code}

\begin{code}
import Monad
import Monoid
import MonadFix
import MonadTrans
import MonadReader
\end{code}

\haskell:class{
  \Name{MonadWriter}
  \tell{tell is like tell on the MUD's it shouts to monad
	what you want to be heard. The monad carries this 'packet'
	upwards, merging it if needed (hence the Monoid requirement)}
  \listen{listen listens to a monad acting, and returns what
	  the monad "said"}.
  \pass{pass lets you provide a writer transformer,
	which changes internals of the written object}
}

\begin{code}
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
	tell   :: w -> m ()
	listen :: m a -> m (a, w)
	pass   :: m (a, w -> w) -> m a
\end{code}

\begin{code}
listens :: (MonadWriter w m) => (w -> w) -> m a -> m (a, w)
listens f m = do
	(a, w) <- listen m
	return (a, f w)

censor :: (MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
	a <- m
	return (a, f)
\end{code}

\h2{Our parameterizable writer monad}

\begin{code}
newtype Writer w a = Writer { runWriter :: (a, w) }
\end{code}

\begin{code}
instance Functor (Writer w) where
	fmap f m = Writer $ let (a, w) = runWriter m in (f a, w)

instance (Monoid w) => Monad (Writer w) where
	return a = Writer (a, mempty)
	m >>= k  = Writer $ let
		(a, w)  = runWriter m
		(b, w') = runWriter (k a)
		in (b, w `mappend` w')

instance (Monoid w) => MonadFix (Writer w) where
	mfix m = Writer $ let (a, w) = runWriter (m a) in (a, w)

instance (Monoid w) => MonadWriter w (Writer w) where
	tell   w = Writer ((), w)
	listen m = Writer $ let (a, w) = runWriter m in ((a, w), w)
	pass   m = Writer $ let ((a, f), w) = runWriter m in (a, f w)
\end{code}

\begin{code}
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)
\end{code}

\h2{Our parameterizable writer monad, with an inner monad}

\begin{code}
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
\end{code}

\begin{code}
instance (Monad m) => Functor (WriterT w m) where
	fmap f m = WriterT $ do
		(a, w) <- runWriterT m
		return (f a, w)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
	return a = WriterT $ return (a, mempty)
	m >>= k  = WriterT $ do
		(a, w)  <- runWriterT m
		(b, w') <- runWriterT (k a)
		return (b, w `mappend` w')
	fail msg = WriterT $ fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
	mzero       = WriterT mzero
	m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
	mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
	tell   w = WriterT $ return ((), w)
	listen m = WriterT $ do
		(a, w) <- runWriterT m
		return ((a, w), w)
	pass   m = WriterT $ do
		((a, f), w) <- runWriterT m
		return (a, f w)

instance (Monoid w) => MonadTrans (WriterT w) where
	lift m = WriterT $ do
		a <- m
		return (a, mempty)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
	liftIO = lift . liftIO

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
	ask       = lift ask
	local f m = WriterT $ local f (runWriterT m)
\end{code}

\begin{code}
execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
	(_, w) <- runWriterT m
	return w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)
\end{code}

\h2{MonadWriter instances for other monad transformers}

\begin{code}
instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
	tell     = lift . tell
	listen m = ReaderT $ \w -> listen (runReaderT m w)
	pass   m = ReaderT $ \w -> pass   (runReaderT m w)
\end{code}
